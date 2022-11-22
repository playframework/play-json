/*
 * Copyright (C) 2009-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import play.api.libs.json.jackson.JacksonJson

object StaticBinding {

  /** Parses a [[JsValue]] from raw data. */
  def parseJsValue(data: Array[Byte]): JsValue =
    JacksonJson.instance.parseJsValue(data)

  /** Parses a [[JsValue]] from a string content. */
  def parseJsValue(input: String): JsValue =
    JacksonJson.instance.parseJsValue(input)

  /** Parses a [[JsValue]] from a stream. */
  def parseJsValue(stream: java.io.InputStream): JsValue =
    JacksonJson.instance.parseJsValue(stream)

  def generateFromJsValue(jsValue: JsValue, escapeNonASCII: Boolean): String =
    JacksonJson.instance.generateFromJsValue(jsValue, escapeNonASCII)

  def prettyPrint(jsValue: JsValue): String = JacksonJson.instance.prettyPrint(jsValue)

  def toBytes(jsValue: JsValue): Array[Byte] =
    JacksonJson.instance.jsValueToBytes(jsValue)
}
