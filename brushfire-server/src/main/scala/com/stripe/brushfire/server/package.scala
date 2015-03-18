package com.stripe.brushfire

import scala.language.higherKinds

import scala.collection.generic.CanBuildFrom
import scala.util.{ Try => ScalaTry, Success, Failure }
import com.twitter.util.{ Try => TwitterTry, Return, Throw }

package object server {

  implicit class ScalaTryConverters[A](val scalaTry: ScalaTry[A]) extends AnyVal {
    def toTwitterTry: TwitterTry[A] = scalaTry match {
      case Success(a) => Return(a)
      case Failure(e) => Throw(e)
    }
  }

  implicit class TwitterTryConverters[A](val twitterTry: TwitterTry[A]) extends AnyVal {
    def toScalaTry: ScalaTry[A] = twitterTry match {
      case Return(a) => Success(a)
      case Throw(e) => Failure(e)
    }
  }

  implicit class ScalaTryCompanionOps(val comp: ScalaTry.type) extends AnyVal {

    /**
     * Convert a sequence of `Try`s to a `Try` of a sequence. Serves the same
     * purpose as Twitter's `Try`'s/`Future`'s `collect`, or Scala's `Future`'s
     * `sequence`.
     *
     * @usecase def sequence(that: Seq[Try[A]]): Try[Seq[A]]
     */
    def sequence[CC[X] <: TraversableOnce[X], A, That](ts: CC[ScalaTry[A]])(implicit cbf: CanBuildFrom[CC[ScalaTry[A]], A, That]): ScalaTry[That] = {
      val bldr = cbf()
      ts.foreach { t =>
        t match {
          case Success(a) =>
            bldr += a
          case Failure(e) =>
            return Failure(e)
        }
      }
      Success(bldr.result())
    }
  }
}
