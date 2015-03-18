package com.stripe.brushfire
package finatra

import scala.util.Try

import com.twitter.bijection.Injection

/**
 * A trait to define different strategies to decode feature vectors from
 * parameter maps.
 */
trait FeatureExtractor[K, V] { self =>

  /**
   * Returns an extraction function for the forest. The function returned must
   * only be used with the forest it was created from, otherwise the results
   * are undefined. For instance, the [[DispatchedFeatureExtractor]] uses the
   * forest to determine which [[Dispatched]] sub-type each feature is, so the
   * parser for each parameter value depends upon the key it is associated
   * with.
   */
  def forForest[T](trees: => Iterable[Tree[K, V, T]]): Map[String, String] => Try[Map[K, V]]

  /** Map the keys in the extracted map using the function `f`. */
  def mapKeys[K1](implicit inj: Injection[K1, K]): FeatureExtractor[K1, V] =
    new FeatureExtractor[K1, V] {
      def forForest[T](trees: => Iterable[Tree[K1, V, T]]): Map[String, String] => Try[Map[K1, V]] = {
        val f = self.forForest(trees.map(_.mapKeys(inj(_))))

        { params =>
          for {
            row <- f(params)
            kvs <- Try.sequence(row.map { case (k, v) => inj.invert(k).map(_ -> v) })
          } yield kvs.toMap
        }
      }
    }

  /** Map the values in the extracted map using the function `f`. */
  def mapValues[V1](implicit inj: Injection[V1, V], ord: Ordering[V]): FeatureExtractor[K, V1] = 
    new FeatureExtractor[K, V1] {
      def forForest[T](trees: => Iterable[Tree[K, V1, T]]): Map[String, String] => Try[Map[K, V1]] = {
        val f = self.forForest(trees.map(_.mapPredicates(inj(_))))

        { params =>
          for {
            row    <- f(params)
            kvs    <- Try.sequence(row.map { case (k, v) => inj.invert(v).map(k -> _) })
          } yield kvs.toMap
        }
      }
    }
}

object IdentityFeatureExtractor extends FeatureExtractor[String, String] {
  def forForest[T](trees: => Iterable[Tree[String, String, T]]): Map[String, String] => Try[Map[String, String]] =
    scala.util.Success(_)
}

class DispatchedFeatureExtractor[K, A, B, C, D, V](prev: FeatureExtractor[K, V])(implicit
  injA: Injection[A, V],
  injB: Injection[B, V],
  injC: Injection[C, V],
  injD: Injection[D, V],
  ord: Ordering[V]) extends FeatureExtractor[K, Dispatched[A, B, C, D]] {

  def forForest[T](getTrees: => Iterable[Tree[K, Dispatched[A, B, C, D], T]]): Map[String, String] => Try[Map[K, Dispatched[A, B, C, D]]] = {

    // Attempts to parse a value to a Dispatched type.
    type Parser = V => Try[Dispatched[A, B, C, D]]
  
    // All the possible parsers we may encounter.
    val ordinalParser: Parser = { v => injA.invert(v).map(Dispatched.ordinal) }
    val nominalParser: Parser = { v => injB.invert(v).map(Dispatched.nominal) }
    val continuousParser: Parser = { v => injC.invert(v).map(Dispatched.continuous) }
    val sparseParser: Parser = { v => injD.invert(v).map(Dispatched.sparse) }
  
    // Given some predicate for a feature, try to guess the type of the
    // feature's values from the leaf-level predicates (EqualTo/LessThan), then
    // return a Parser for that Dispatched type.
    def parserFor(pred: Predicate[Dispatched[A, B, C, D]]): Option[Parser] = pred match {
      case EqualTo(Ordinal(_)) | LessThan(Ordinal(_)) => Some(ordinalParser)
      case EqualTo(Nominal(_)) | LessThan(Nominal(_)) => Some(nominalParser)
      case EqualTo(Continuous(_)) | LessThan(Continuous(_)) => Some(continuousParser)
      case EqualTo(Sparse(_)) | LessThan(Sparse(_)) => Some(sparseParser)
      case Not(p) => parserFor(p)
      case AnyOf(ps) =>
        ps.toIterator.map(parserFor).collectFirst { case Some(p) => p }
    }
  
    // For a given tree, we try to find any new features we haven't seen yet,
    // infer their Dispatched type, then update the map to parse that feature
    // with the parser for the inferred Dispatch type.
    def collect(node: Node[K, Dispatched[A, B, C, D], T], initGuesses: Map[K, Parser]): Map[K, Parser] =
      node match {
        case SplitNode(children) =>
          children.foldLeft(initGuesses) { case (acc, (key, pred, child)) =>
            val guesses = if (!acc.contains(key)) {
              parserFor(pred)
                .map(parser => acc + (key -> parser))
                .getOrElse(acc)
            } else {
              acc
            }
            collect(child, guesses)
          }
  
        case LeafNode(_, _) => initGuesses
      }

    val trees = getTrees
  
    val parsers = trees.foldLeft(Map.empty[K, Parser]) { (ps, tree) =>
      collect(tree.root, ps)
    }

    val initExtractor = prev.forForest(trees.map(_.mapPredicates {
      case Ordinal(a) => injA(a)
      case Nominal(b) => injB(b)
      case Continuous(c) => injC(c)
      case Sparse(d) => injD(d)
    }))

    def parseDispatched(row: Map[K, V]): Map[K, Dispatched[A, B, C, D]] = for {
      (k, parser) <- parsers
      value       <- row.get(k)
      dispatched  <- parser(value).toOption // Treating errors as missing values!
    } yield {
      k -> dispatched
    }

    { params => initExtractor(params).map(parseDispatched) }
  }
}
