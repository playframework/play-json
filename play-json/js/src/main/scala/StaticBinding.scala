/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import scalajs.js
import js.JSON

object StaticBinding {

  /** Parses a [[JsValue]] from raw data (assuming UTF-8). */
  def parseJsValue(data: Array[Byte]): JsValue =
    parseJsValue(new String(data, "UTF-8"))

  /** Parses a [[JsValue]] from a stream (assuming UTF-8). */
  def parseJsValue(stream: java.io.InputStream): JsValue =
    StaticBindingNonJvm.parseJsValue(stream)

  /** Parses a [[JsValue]] from a string content. */
  def parseJsValue(input: String): JsValue =
    anyToJsValue(JSON.parse(input))

  def generateFromJsValue(jsValue: JsValue, escapeNonASCII: Boolean): String =
    StaticBindingNonJvm.generateFromJsValue(jsValue, escapeNonASCII)

  def prettyPrint(jsValue: JsValue): String = StaticBindingNonJvm.prettyPrint(jsValue)

  def toBytes(jsValue: JsValue): Array[Byte] = StaticBindingNonJvm.toBytes(jsValue)

  @inline private[json] def fromString(s: String, escapeNonASCII: Boolean): String =
    if (!escapeNonASCII) JSON.stringify(s, null) else StaticBindingNonJvm.escapeStr(JSON.stringify(s, null))

  private def anyToJsValue(raw: Any): JsValue = raw match {
    case null           => JsNull
    case s: String      => JsString(s)
    case d: Double      => JsNumber(d)
    case f: Float       => JsNumber(BigDecimal.decimal(f))
    case i: Int         => JsNumber(i)
    case l: Long        => JsNumber(l)
    case true           => JsTrue
    case false          => JsFalse
    case a: js.Array[?] => JsArray(a.map(anyToJsValue).toArray[JsValue])

    case o: js.Object => {
      JsObject((for {
        (k, v) <- o.asInstanceOf[js.Dictionary[js.Any]]
      } yield k -> anyToJsValue(v)).toSeq)
    }

    case _ => sys.error(s"Unexpected JS value: $raw")
  }
}
