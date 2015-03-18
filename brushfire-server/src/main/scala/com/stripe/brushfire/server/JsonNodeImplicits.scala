package com.stripe.brushfire
package server

import scala.util.{ Try, Success, Failure }

import org.codehaus.jackson.JsonNode
import org.codehaus.jackson.map.JsonMappingException
import org.codehaus.jackson.JsonParser.NumberType

object JsonNodeImplicits {
  implicit class JsonNodeOps(val node: JsonNode) extends AnyVal {
    def tryField(field: String): Try[JsonNode] = {
      val valueNode = node.get(field)
      if (valueNode != null) Success(valueNode)
      else Failure(new JsonMappingException(s"missing field: $field"))
    }

    def tryAsText: Try[String] = {
      val value = node.getTextValue
      if (value != null) Success(value)
      else Failure(new JsonMappingException(s"expected text: $node"))
    }

    def tryAsInt: Try[Int] =
      if (node.isNumber) {
        node.getNumberType match {
          case NumberType.INT => Success(node.getIntValue)
          case _ => Failure(new JsonMappingException(s"not an integer: ${node.getNumberValue}"))
        }
      } else {
        Failure(new JsonMappingException(s"not a number: $node"))
      }
  }
}
