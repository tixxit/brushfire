package com.stripe.brushfire
package server

import scala.util.Try

import com.twitter.bijection._
import com.twitter.bijection.json.JsonNodeInjection
import com.twitter.bijection.json.JsonNodeInjection._
import com.twitter.util.Future

import org.codehaus.jackson.JsonNode

import com.stripe.brushfire.server.forest.ForestModelTypeBuilder

/**
 * A wrapper for a specific model type. The model type itself is existential,
 * so the only way to obtain instances is by using either [[storeCodec]]
 * with some parameterized serialized type `A`, or using the [[codec]].
 */
trait ModelType[A] {

  /**
   * We don't *really* care about the specific type (and aren't afforded the
   * luxury to, regardless), but we do care that the results are returned as
   * JSON.
   *
   * @note If we want to support other content-types in the future, then we can
   *       leave the prediction/scoring type abstract, and keep a dictionary of
   *       codecs by content type to serialize the scores as well.
   */
  type JsonModel <: Model[JsonNode]

  /**
   * An injection to map models to some underlying serialization type (eg.
   * `Array[Byte]`, `String`, `JsonNode`, etc).
   */
  def storeCodec: Injection[JsonModel, A]

  /**
   * A codec to parse models from an array of bytes. Currently this is in
   * some fixed format per model type (eg the TSV format from the Scalding
   * Trainer).
   *
   * @note In the future we may want to make this a dictionary, keyed by
   *       content-type to support other formats.
   */
  def codec: Codec[JsonModel]
}

object ModelType {

  /**
   * Returns a builder for constructing [[ModelType]]s for Brushfire random
   * forests.
   */
  def forestBuilder: ForestModelTypeBuilder[String, String, Map[String, Long], Map[String, Double]] =
    ForestModelTypeBuilder()
}

class JsonModelType[P, M <: Model[P], A](storeCodec0: Injection[M, A], codec0: Codec[M], jsonInj: JsonNodeInjection[P])
extends ModelType[A] {
  case class JsonModel(model: M) extends Model[JsonNode] {
    def metadata: ModelMetadata = model.metadata
    def score(params: Map[String, String]): Future[JsonNode] =
      model.score(params).map(jsonInj(_))
  }

  val jsonModelBijection: Bijection[JsonModel, M] =
    Bijection.build[JsonModel, M](_.model)(JsonModel(_))

  val storeCodec = storeCodec0 compose jsonModelBijection
  val codec = codec0 compose jsonModelBijection
}
