package com.stripe.brushfire
package server
package forest

import com.twitter.bijection._
import com.twitter.bijection.json.JsonNodeInjection
import com.twitter.bijection.json.JsonNodeInjection._
import com.twitter.util.Future

import org.codehaus.jackson.JsonNode

class ForestModelTypeBuilder[K, V, T, P](
  voter: Voter[T, Future[P]],
  extractor: FeatureExtractor[K, V]
) {
  // TODO: mapKeys/mapValues are not named well - mapping params, not tree!

  def mapKeys[K1](onFailure: OnFailure = OnFailure.Default)(implicit inj: Injection[K1, K]): ForestModelTypeBuilder[K1, V, T, P] =
    new ForestModelTypeBuilder(voter, extractor.mapKeys(onFailure)(inj))

  def mapValues[V1](onFailure: OnFailure = OnFailure.Default)(implicit inj: Injection[V1, V], ord: Ordering[V]): ForestModelTypeBuilder[K, V1, T, P] =
    new ForestModelTypeBuilder(voter, extractor.mapValues(onFailure)(inj, ord))

  def withVoter[T1, P1](voter1: Voter[T1, Future[P1]]): ForestModelTypeBuilder[K, V, T1, P1] =
    new ForestModelTypeBuilder(voter1, extractor)

  def dispatched[A, B, C, D](onFailure: OnFailure = OnFailure.Default)(implicit
      injA: Injection[A, V],
      injB: Injection[B, V],
      injC: Injection[C, V],
      injD: Injection[D, V],
      ord: Ordering[V]): ForestModelTypeBuilder[K, Dispatched[A, B, C, D], T, P] =
    new ForestModelTypeBuilder(voter, new DispatchedFeatureExtractor(extractor, onFailure)(injA, injB, injC, injD, ord))

  def json()(implicit
    jsonInj: JsonNodeInjection[Tree[K, V, T]],
    strInj: Injection[Tree[K, V, T], String],
    predInj: JsonNodeInjection[P]): ModelType[JsonNode] = {
    val injections = new ForestModelInjections(extractor, voter)
    new JsonModelType[P, ForestModel[K, V, T, P], JsonNode](injections.jsonInjection, injections.scaldingCodec, predInj)
  }
}

object ForestModelTypeBuilder {
  def apply(): ForestModelTypeBuilder[String, String, Map[String, Long], Map[String, Double]] =
    new ForestModelTypeBuilder(SoftVoter[String, Long].map(Future.value(_)), IdentityFeatureExtractor)
}
