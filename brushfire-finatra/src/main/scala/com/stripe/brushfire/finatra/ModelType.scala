package com.stripe.brushfire
package finatra

import scala.util.Try

import com.twitter.bijection._
import com.twitter.bijection.json.JsonNodeInjection
import com.twitter.bijection.json.JsonNodeInjection._
import com.twitter.util.Future

import org.codehaus.jackson.JsonNode

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
  def brushfireBuilder: BrushfireBuilder[String, String, Map[String, Long], Map[String, Double]] =
    new BrushfireBuilder(SoftVoter[String, Long].map(Future.value(_)), IdentityFeatureExtractor)

  class BrushfireBuilder[K, V, T, P](
    voter: Voter[T, Future[P]],
    extractor: FeatureExtractor[K, V]
  ) {
    // TODO: mapKeys/mapValues are not named well - mapping params, not tree!

    def mapKeys[K1](implicit inj: Injection[K1, K]): BrushfireBuilder[K1, V, T, P] =
      new BrushfireBuilder(voter, extractor.mapKeys(inj))

    def mapValues[V1](implicit inj: Injection[V1, V], ord: Ordering[V]): BrushfireBuilder[K, V1, T, P] =
      new BrushfireBuilder(voter, extractor.mapValues(inj, ord))

    def withVoter[T1, P1](voter1: Voter[T1, Future[P1]]): BrushfireBuilder[K, V, T1, P1] =
      new BrushfireBuilder(voter1, extractor)

    def dispatched[A, B, C, D](implicit
        injA: Injection[A, V],
        injB: Injection[B, V],
        injC: Injection[C, V],
        injD: Injection[D, V],
        ord: Ordering[V]): BrushfireBuilder[K, Dispatched[A, B, C, D], T, P] =
      new BrushfireBuilder(voter, new DispatchedFeatureExtractor(extractor)(injA, injB, injC, injD, ord))

    def json()(implicit
      jsonInj: JsonNodeInjection[Tree[K, V, T]],
      strInj: Injection[Tree[K, V, T], String],
      predInj: JsonNodeInjection[P]): ModelType[JsonNode] = {
      val injections = new ForestModelInjections(extractor, voter)
      new JsonModelType[P, ForestModel[K, V, T, P], JsonNode](injections.jsonInjection, injections.scaldingCodec, predInj)
    }
  }
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
