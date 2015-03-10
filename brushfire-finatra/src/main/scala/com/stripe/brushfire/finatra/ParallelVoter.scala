package com.stripe.brushfire
package finatra

import com.twitter.util.{ Future, FuturePool }

/**
 * Wraps a voter and evalutes each tree in parallel before combining.
 */
final case class ParallelVoter[T, P](voter: Voter[T, P])(implicit pool: FuturePool) extends Voter[T, Future[P]] {
  override def predict[K, V](trees: Iterable[Tree[K, V, T]], row: Map[K, V]): Future[P] =
    Future
      .collect(trees.toSeq.map { tree => pool(tree.targetFor(row)) })
      .map { targets => voter.combine(targets.flatten) }

  def combine(targets: Iterable[T]): Future[P] =
    Future.value(voter.combine(targets))
}
