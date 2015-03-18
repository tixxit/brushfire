package com.stripe.brushfire
package finatra

import scala.util.Try

import com.twitter.bijection.json.JsonNodeInjection._
import com.twitter.finatra.FinatraServer
import com.twitter.storehaus.ConcurrentHashMapStore
import com.twitter.util.{ Future, Await }

import org.codehaus.jackson.JsonNode

object Main {
  import JsonInjections._

  val forestModelType: ModelType[JsonNode] =
    ModelType.brushfireBuilder
      .mapValues[Double]
      .json()

  val modelStore = JsonModelStore(
    new ConcurrentHashMapStore,
    Map("forest" -> forestModelType))

  def main(args: Array[String]) {
    val server = new FinatraServer
    server.register(new ModelController(modelStore))
    server.main
  }
}
