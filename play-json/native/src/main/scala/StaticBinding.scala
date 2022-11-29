/*
 * Copyright (C) 2009-2021 Lightbend Inc. <https://www.lightbend.com>
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
    StaticBindingJsNative.parseJsValue(stream)

  def generateFromJsValue(jsValue: JsValue, escapeNonASCII: Boolean): String =
    StaticBindingJsNative.generateFromJsValue(jsValue, escapeNonASCII)

  def prettyPrint(jsValue: JsValue): String = StaticBindingJsNative.prettyPrint(jsValue)

  def toBytes(jsValue: JsValue): Array[Byte] = StaticBindingJsNative.toBytes(jsValue)

  @inline private def stringify(s: String) = {
    def escaped(c: Char) = c match {
      case '\b' => "\\b"
      case '\f' => "\\f"
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case '\\' => "\\\\"
      case '\"' => "\\\""
      case c => c.toString
    }
    if (s == null) "null" else s"\"${s.flatMap(escaped)}\""
  }

  @inline private[json] def fromString(s: String, escapeNonASCII: Boolean): String = {
    if (!escapeNonASCII) stringify(s) else StaticBindingJsNative.escapeStr(stringify(s))
  }

}
