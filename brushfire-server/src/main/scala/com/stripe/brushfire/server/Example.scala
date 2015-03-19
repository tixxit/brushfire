package com.stripe.brushfire
package server

import scala.util.Try

import com.twitter.bijection.json.JsonNodeInjection._
import com.twitter.finatra.FinatraServer
import com.twitter.storehaus.ConcurrentHashMapStore
import com.twitter.util.{ Future, Await }

import org.codehaus.jackson.JsonNode

import com.stripe.brushfire.server.finatra.ModelController

object Main {
  import JsonInjections._

  val modelStore = JsonModelStore(
    new ConcurrentHashMapStore,
    Map(
      "simple" -> ModelType.forestBuilder.mapValues[Double](OnFailure.Fail).json(),
      "dispatched" -> ModelType.forestBuilder.dispatched[Long, String, Double, Boolean](OnFailure.Drop).json()))

  def main(args: Array[String]) {
    val server = new FinatraServer
    server.register(new ModelController(modelStore))
    server.main
  }
}
