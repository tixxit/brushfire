package com.stripe.brushfire
package server

import java.util.Date
import java.text.SimpleDateFormat

import scala.util.{ Try, Success, Failure }

import com.twitter.bijection.json._
import com.twitter.bijection.json.JsonNodeInjection._
import com.twitter.util.Future

import org.codehaus.jackson.JsonNode

/**
 * A predictive model.
 */
trait Model[P] { self =>

  /** Return the metadata attached to the model. */
  def metadata: ModelMetadata

  /**
   * Attempt to parse the feature vector from `params` and score it against
   * the model.
   *
   * @param params the unparsed feature vector
   * @return the prediction made, eventually
   */
  def score(params: Map[String, String]): Future[P]
}

case class ModelMetadata(
  lastModified: Option[Date])

object ModelMetadata {
  implicit val ModelMetadataToJson: JsonNodeInjection[ModelMetadata] = new JsonNodeInjection[ModelMetadata] {
    private val DateFmt = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSXXX")

    def apply(metadata: ModelMetadata): JsonNode = {
      toJsonNode(metadata.lastModified.map { date =>
        Map("lastModified" -> toJsonNode(DateFmt.format(date)))
      }.getOrElse {
        Map.empty[String, JsonNode]
      })
    }

    def invert(metadata: JsonNode): Try[ModelMetadata] = for {
      lastModified <- Option(metadata.get("lastModified")) match {
        case Some(dateNode) =>
          fromJsonNode[String](dateNode)
            .flatMap { dateStr => Try(DateFmt.parse(dateStr)) }
            .map(Some(_))
        case None =>
          Success(None)
      }
    } yield {
      ModelMetadata(lastModified)
    }
  }
}

case class ModelResponse(
  name: String,
  metadata: ModelMetadata)

object ModelResponse {
  implicit val ModelResponseToJson: JsonNodeInjection[ModelResponse] = new JsonNodeInjection[ModelResponse] {
    def apply(response: ModelResponse): JsonNode =
      toJsonNode(Map(
        "name" -> toJsonNode(response.name),
        "metadata" -> toJsonNode(response.metadata)))

    private def notNull[A](a: A): Try[A] =
      if (a != null) Success(a)
      else Failure(new NullPointerException("expected non-null"))

    def invert(n: JsonNode): Try[ModelResponse] = for {
      nameNode <- notNull(n.get("name"))
      name <- fromJsonNode[String](nameNode)
      metadataNode <- notNull(n.get("metadata"))
      metadata <- fromJsonNode[ModelMetadata](metadataNode)
    } yield {
      ModelResponse(name, metadata)
    }
  }
}