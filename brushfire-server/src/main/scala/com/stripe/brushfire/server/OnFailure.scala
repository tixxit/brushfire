package com.stripe.brushfire
package server

import scala.util.Try

/**
 * A set of strategies to allow the user to decide whether to ignore errors or
 * not.
 */
sealed abstract class OnFailure(passthrough: Boolean) {

  /**
   * Given a possibly failing result, this will either return the failing
   * result, wrapped in a [[Some]], or drop it by returning [[None]].
   */
  def apply[A](result: Try[A]): Option[Try[A]] =
    if (result.isSuccess || passthrough) Some(result) else None
}

object OnFailure {
  /** The default strategy for dealing failures is to ignore them. */
  val Default: OnFailure = Drop

  /**
   * This will propagate errors out, so that if extracting a feature fails,
   * then the entire feature vector extraction fails.
   */
  case object Fail extends OnFailure(true)

  /**
   * This will drop any errors in individual feature extraction, treating the
   * failing feature as missing.
   */
  case object Drop extends OnFailure(false)
}
