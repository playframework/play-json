/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import org.typelevel.jawn

object StaticBinding {

  private implicit object JsValueFacade extends jawn.Facade.SimpleFacade[JsValue] {
    final def jfalse: JsValue = JsFalse
    final def jnull: JsValue  = JsNull
    final def jnum(s: CharSequence, decIndex: Int, expIndex: Int): JsValue = JsNumber(
      new java.math.BigDecimal(s.toString)
    )
    final def jstring(s: CharSequence): JsValue = JsString(s.toString)
    final def jtrue: JsValue                    = JsTrue

    final def jarray(vs: List[JsValue]): JsValue         = JsArray(vs)
    final def jobject(vs: Map[String, JsValue]): JsValue = JsObject(vs)
  }

  /** Parses a [[JsValue]] from raw data (assuming UTF-8). */
  def parseJsValue(data: Array[Byte]): JsValue =
    new jawn.ByteArrayParser[JsValue](data).parse()

  /** Parses a [[JsValue]] from a string content. */
  def parseJsValue(input: String): JsValue =
    jawn.Parser.parseUnsafe[JsValue](input)

  /** Parses a [[JsValue]] from a stream (assuming UTF-8). */
  def parseJsValue(stream: java.io.InputStream): JsValue =
    StaticBindingNonJvm.parseJsValue(stream)

  def generateFromJsValue(jsValue: JsValue, escapeNonASCII: Boolean): String =
    StaticBindingNonJvm.generateFromJsValue(jsValue, escapeNonASCII)

  def prettyPrint(jsValue: JsValue): String = StaticBindingNonJvm.prettyPrint(jsValue)

  def toBytes(jsValue: JsValue): Array[Byte] = StaticBindingNonJvm.toBytes(jsValue)

  @inline private[json] def fromString(s: String, escapeNonASCII: Boolean): String = {
    def escaped(c: Char) = c match {
      case '\b' => "\\b"
      case '\f' => "\\f"
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case '\\' => "\\\\"
      case '\"' => "\\\""
      case c    => c.toString
    }
    val stringified = if (s == null) "null" else s""""${s.flatMap(escaped)}""""
    if (!escapeNonASCII) stringified else StaticBindingNonJvm.escapeStr(stringified)
  }

}
