package com.stripe.brushfire
package server

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
   * Deserialize and add a model to the store, specifying the expected model
   * type.
   *
   * @param name      the key to associate the value with
   * @param modelType the name of the [[ModelType]] to treat the model as
   * @param data      serialized version of the model
   * @return the deserialized model stored, if successful
   */
  def put(name: String, modelType: String, data: Array[Byte]): Future[Model[A]]

  /**
   * Deserialize and add a model to the store. Note that a model store may
   * require a model type in order to be able to deserialize the model
   * correctly. If that's the case, this will fail with an
   * `InvalidModelException`.
   *
   * @param name      the key to associate the value with
   * @param data      serialized version of the model
   * @return the deserialized model stored, if successful
   */
  def put(name: String, data: Array[Byte]): Future[Model[A]]
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

  private def tryParse(modelType: ModelType[JsonNode], data: Array[Byte]): Try[(Model[JsonNode], JsonNode)] = {
    modelType.codec.invert(data) match {
      case Success(m) =>
        Success(m -> modelType.storeCodec(m))
      case Failure(cause) =>
        Try { throw new ModelStore.InvalidModelException(null, cause) }
    }
  }

  private def storeAsJson(name: String, modelType: String, model: JsonNode): Future[Unit] = {
    val json = toJsonNode(Map(
      "modelType" -> toJsonNode(modelType),
      "model" -> model))
    store.put(name -> Some(json))
  }

  def put(name: String, data: Array[Byte]): Future[Model[JsonNode]] = {
    val parseResult: List[(String, Model[JsonNode], JsonNode)] = modelTypes
      .map { case (typeName, modelType) =>
        tryParse(modelType, data).map { case (model, jsonModel) =>
          (typeName, model, jsonModel)
        }
      }
      .collect { case Success(triplet) => triplet } (collection.breakOut)

    parseResult match {
      case Nil =>
        // No model types matched.
        Future { throw new ModelStore.InvalidModelException("could not match model with any model type", null) }
      case (typeName, model, jsonModel) :: Nil =>
        // The good case: exactly 1 model type matched.
        storeAsJson(name, typeName, jsonModel).map { _ => model }
      case results =>
        // More than 1 model type matched.
        val modelTypes = results.map(_._1).mkString(", ")
        Future { throw new ModelStore.InvalidModelException("model matches multiple model types: $modelTypes", null) }
    }
  }

  /**
   * Associate the model with type `modelTypeName` to the given name.
   */
  def put(name: String, modelTypeName: String, data: Array[Byte]): Future[Model[JsonNode]] =
    modelTypes.get(modelTypeName) match {
      case Some(modelType) =>
        for {
          (model, jsonModel) <- Future.const(tryParse(modelType, data).toTwitterTry)
          _                  <- storeAsJson(name, modelTypeName, jsonModel)
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
