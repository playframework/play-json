/*
 * Copyright (C) Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import play.api.libs.json.jackson.JacksonJson

object StaticBinding {

  /** Parses a [[JsValue]] from raw data. */
  def parseJsValue(data: Array[Byte]): JsValue =
    JacksonJson.get.parseJsValue(data)

  /** Parses a [[JsValue]] from a string content. */
  def parseJsValue(input: String): JsValue =
    JacksonJson.get.parseJsValue(input)

  /** Parses a [[JsValue]] from a stream. */
  def parseJsValue(stream: java.io.InputStream): JsValue =
    JacksonJson.get.parseJsValue(stream)

  def generateFromJsValue(jsValue: JsValue, escapeNonASCII: Boolean): String =
    JacksonJson.get.generateFromJsValue(jsValue, escapeNonASCII)

  def prettyPrint(jsValue: JsValue): String = JacksonJson.get.prettyPrint(jsValue)

  def toBytes(jsValue: JsValue): Array[Byte] =
    JacksonJson.get.jsValueToBytes(jsValue)
}
