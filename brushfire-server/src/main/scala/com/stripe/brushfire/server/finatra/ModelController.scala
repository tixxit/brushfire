package com.stripe.brushfire
package server
package finatra

import scala.collection.concurrent.TrieMap

import com.twitter.bijection._
import com.twitter.bijection.json._
import com.twitter.finatra._
import com.twitter.finagle.http._
import com.twitter.util.{ Try, Return, Throw, Future }

import org.codehaus.jackson.JsonNode

class ModelController(store: ModelStore[JsonNode]) extends Controller {
  import JsonInjections._
  import JsonNodeInjection._

  private def withModel(name: String)(f: Model[JsonNode] => Future[ResponseBuilder]): Future[ResponseBuilder] = {
    store.get(name) flatMap {
      case Some(model) => f(model)
      case None => render.status(404).toFuture
    }
  }

  get("/model/:name") { request =>
    val name = request.routeParams("name")
    withModel(name) { model =>
      render
        .body(toJsonNode(ModelResponse(name, model.metadata)).toString)
        .contentType("application/json")
        .toFuture
    }
  }

  get("/model/:name/score") { request =>
    withModel(request.routeParams("name")) { model =>
      model.score(request.params).map { score =>
        render
          .body(score.toString)
          .contentType("application/json")
      }
    }
  }

  // TIL: Finatra doesn't support multipart PUT requests.
  post("/model/") { request =>
    val response = for {
      name      <- request.multiParams.get("name").map(_.value)
      modelType <- request.multiParams.get("modelType").map(_.value)
      data      <- request.multiParams.get("model").map(_.data)
    } yield {
      store.put(name, modelType, data).transform {
        case Return(model) =>
          redirect(s"/model/$name").toFuture
        case Throw(e: ModelStore.MissingModelTypeException) =>
          render.body(s"invalid modelType: ${e.modelType}").status(400).toFuture
        case Throw(e: ModelStore.InvalidModelException) =>
          render.body(s"couldn't parse model").status(400).toFuture
        case Throw(e) =>
          Future.const(Throw(e))
      }
    }

    response.getOrElse {
      render.body("required fields: modelType, model").status(400).toFuture
    }
  }
}
