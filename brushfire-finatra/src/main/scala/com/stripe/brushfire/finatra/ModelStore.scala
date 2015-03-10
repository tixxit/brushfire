package com.stripe.brushfire
package finatra

import scala.collection.concurrent.TrieMap
import scala.util.{ Try, Success, Failure }

import com.twitter.bijection._
import com.twitter.bijection.json._
import com.twitter.bijection.json.JsonNodeInjection._
import com.twitter.storehaus.{ Store, ReadableStore }
import com.twitter.util.Future

import org.codehaus.jackson.JsonNode

import JsonNodeImplicits._

/**
 * A store for models to be served.
 */
trait ModelStore[A] extends ReadableStore[String, Model[A]] {

  /**
   * Deserialize and add a model to the store.
   *
   * @param name      the key to associate the value with
   * @param modelType the name of the [[ModelType]] to treat the model as
   * @param data      serialized version of the model
   * @return the deserialized model stored, if successful
   */
  def put(name: String, modelType: String, data: Array[Byte]): Future[Model[A]]
}

object ModelStore {

  /**
   * An exception indicating that the model type specified during a `put` was
   * invalid.
   */
  final class MissingModelTypeException(val modelType: String) extends Exception(s"missing modelType: $modelType")

  /**
   * An exception indicating that the model data specified during a `put` could
   * not be parsed.
   */
  final class InvalidModelException(message: String, cause: Throwable) extends Exception(message, cause)
}

/**
 * A [[ModelStore]] that is specialized for storing models and scoring as JSON.
 */
case class JsonModelStore(
  store: Store[String, JsonNode],
  modelTypes: Map[String, ModelType[JsonNode]]
) extends ModelStore[JsonNode] {

  /**
   * Associate the model with type `modelTypeName` to the given name.
   */
  def put(name: String, modelTypeName: String, data: Array[Byte]): Future[Model[JsonNode]] =
    modelTypes.get(modelTypeName) match {
      case Some(modelType) =>
        val parsedModel: Try[modelType.JsonModel] = modelType.codec.invert(data) match {
          case Success(m) => Success(m: modelType.JsonModel)
          case Failure(cause) =>
            Try { throw new ModelStore.InvalidModelException(null, cause) }
        }

        for {
          model <- Future.const(parsedModel.toTwitterTry)
          json   = toJsonNode(Map(
                     "modelType" -> toJsonNode(modelTypeName),
                     "model" -> modelType.storeCodec(model)))
          _     <- store.put(name -> Some(json))
        } yield {
          model
        }

      case None =>
        Future.const { throw new ModelStore.MissingModelTypeException(modelTypeName) }
    }

  /**
   * Returns the model associated with the given name, if one exists.
   */
  override def get(name: String): Future[Option[Model[JsonNode]]] = for {
    node  <- store.get(name)
    model <- node.map(parseModel) match {
               case Some(result) => Future.const(result.map(Some(_)).toTwitterTry)
               case None => Future.None
             }
  } yield model

  private def parseModel(node: JsonNode): Try[Model[JsonNode]] = for {
    typeNameNode <- node.tryField("modelType")
    typeName     <- typeNameNode.tryAsText
    data         <- node.tryField("model")
    modelType    <- Try(modelTypes.get(typeName).getOrElse(throw new ModelStore.MissingModelTypeException(typeName)))
    model        <- modelType.storeCodec.invert(data)
  } yield model
}
