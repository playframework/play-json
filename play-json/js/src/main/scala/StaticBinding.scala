/*
 * Copyright (C) 2009-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import java.io.InputStreamReader

import scalajs.js
import js.JSON

object StaticBinding {

  /** Parses a [[JsValue]] from raw data (assuming UTF-8). */
  def parseJsValue(data: Array[Byte]): JsValue =
    parseJsValue(new String(data, "UTF-8"))

  /** Parses a [[JsValue]] from a stream (assuming UTF-8). */
  def parseJsValue(stream: java.io.InputStream): JsValue = {
    var in: InputStreamReader = null

    try {
      in = new java.io.InputStreamReader(stream, "UTF-8")
      val acc = new StringBuilder()
      val buf = Array.ofDim[Char](1024)

      @annotation.tailrec
      def read(): String = {
        val r = in.read(buf, 0, 1024)

        if (r == 1024) {
          acc ++= buf
          read()
        } else if (r > 0) {
          acc ++= buf.slice(0, r)
          read()
        } else acc.result()
      }

      parseJsValue(read())
    } catch {
      case err: Throwable => throw err
    } finally {
      if (in != null) in.close()
    }
  }

  /** Parses a [[JsValue]] from a string content. */
  def parseJsValue(input: String): JsValue =
    anyToJsValue(JSON.parse(input))

  def generateFromJsValue(jsValue: JsValue, escapeNonASCII: Boolean): String =
    fromJs(jsValue, escapeNonASCII, 0, _ => "")

  def prettyPrint(jsValue: JsValue): String =
    fromJs(
      jsValue,
      false,
      0,
      { l => 0.until(l * 2).map(_ => ' ').mkString },
      newline = true,
      fieldValueSep = " : ",
      arraySep = ("[ ", ", ", " ]")
    )

  def toBytes(jsValue: JsValue): Array[Byte] =
    generateFromJsValue(jsValue, false).getBytes("UTF-8")

  // ---

  private def fromJs(
      jsValue: JsValue,
      escapeNonASCII: Boolean,
      ilevel: Int,
      indent: Int => String,
      newline: Boolean = false,
      fieldValueSep: String = ":",
      arraySep: (String, String, String) = ("[", ",", "]")
  ): String = {
    def str = jsValue match {
      case JsNull      => "null"
      case JsString(s) => fromString(s, escapeNonASCII)
      case JsNumber(n) => n.toString
      case JsTrue      => "true"
      case JsFalse     => "false"

      case JsArray(items) => {
        val il = ilevel + 1

        items
          .map(fromJs(_, escapeNonASCII, il, indent, newline, fieldValueSep, arraySep))
          .mkString(arraySep._1, arraySep._2, arraySep._3)
      }

      case JsObject(fields) => {
        val il = ilevel + 1
        val (before, after) = if (newline) {
          s"\n${indent(il)}" -> s"\n${indent(ilevel)}}"
        } else indent(il) -> "}"

        fields.map {
          case (k, v) =>
            @inline def key   = fromString(k, escapeNonASCII)
            @inline def value = fromJs(v, escapeNonASCII, il, indent, newline, fieldValueSep, arraySep)

            s"$before$key$fieldValueSep$value"
        }.mkString("{", ",", after)
      }
    }

    str
  }

  @inline private def fromString(s: String, escapeNonASCII: Boolean): String =
    if (!escapeNonASCII) JSON.stringify(s, null) else escapeStr(JSON.stringify(s, null))

  private def anyToJsValue(raw: Any): JsValue = raw match {
    case null           => JsNull
    case s: String      => JsString(s)
    case d: Double      => JsNumber(d)
    case f: Float       => JsNumber(BigDecimal.decimal(f))
    case i: Int         => JsNumber(i)
    case l: Long        => JsNumber(l)
    case true           => JsTrue
    case false          => JsFalse
    case a: js.Array[_] => JsArray(a.map(anyToJsValue).toArray[JsValue])

    case o: js.Object => {
      JsObject((for {
        (k, v) <- o.asInstanceOf[js.Dictionary[js.Any]]
      } yield k -> anyToJsValue(v)).toSeq)
    }

    case _ => sys.error(s"Unexpected JS value: $raw")
  }

  private def escapeStr(s: String): String = s.flatMap { c =>
    val code = c.toInt

    if (code > 31 && code < 127 /* US-ASCII */ ) String.valueOf(c)
    else {
      def hexCode = code.toHexString.reverse.padTo(4, '0').reverse
      '\\' +: s"u${hexCode.toUpperCase}"
    }
  }
}
