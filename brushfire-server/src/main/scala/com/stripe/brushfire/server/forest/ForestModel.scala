package com.stripe.brushfire
package server
package forest

import scala.util.Try

import com.twitter.util.Future

/**
 * A model that uses a Brushfire random forest for predictions.
 *
 * @param trees    a random forest
 * @param voter    voter used to make predictions
 * @param metadata metadata to associate with this model
 * @param extract  a function to parse feature vectors
 */
case class ForestModel[K, V, T, P](
    trees: Map[Int, Tree[K, V, T]],
    voter: Voter[T, Future[P]],
    metadata: ModelMetadata,
    extract: Map[String, String] => Try[Map[K, V]]) extends Model[P] {
  def score(params: Map[String, String]): Future[P] = for {
    fv <- Future.const(extract(params).toTwitterTry)
    score <- voter.predict(trees.values, fv)
  } yield score
}
