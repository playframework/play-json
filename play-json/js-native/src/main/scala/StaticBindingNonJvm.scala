/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import java.io.InputStreamReader

private[json] object StaticBindingNonJvm {

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

      StaticBinding.parseJsValue(read())
    } catch {
      case err: Throwable => throw err
    } finally {
      if (in != null) in.close()
    }
  }

  def generateFromJsValue(jsValue: JsValue, escapeNonASCII: Boolean): String =
    fromJs(jsValue, escapeNonASCII, 0, _ => "")

  def prettyPrint(jsValue: JsValue): String =
    fromJs(
      jsValue,
      false,
      0,
      { l =>
        0.until(l * 2).map(_ => ' ').mkString
      },
      newline = true,
      fieldValueSep = " : ",
      arraySep = ("[ ", ", ", " ]")
    )

  // TODO: Write to the stream when traversing JsValue without buffering the whole string.
  def prettyPrintToStream(jsValue: JsValue, stream: java.io.OutputStream): Unit =
    stream.write(prettyPrint(jsValue).getBytes("UTF-8"))

  def toBytes(jsValue: JsValue): Array[Byte] =
    generateFromJsValue(jsValue, false).getBytes("UTF-8")

  // TODO: Write to the stream when traversing JsValue without buffering the whole string.
  def writeToStream(jsValue: JsValue, stream: java.io.OutputStream): Unit =
    stream.write(toBytes(jsValue))

  def fromJs(
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
      case JsString(s) => StaticBinding.fromString(s, escapeNonASCII)
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
        val il              = ilevel + 1
        val (before, after) = if (newline) {
          s"\n${indent(il)}" -> s"\n${indent(ilevel)}}"
        } else indent(il) -> "}"

        fields
          .map { case (k, v) =>
            @inline def key   = StaticBinding.fromString(k, escapeNonASCII)
            @inline def value = fromJs(v, escapeNonASCII, il, indent, newline, fieldValueSep, arraySep)

            s"$before$key$fieldValueSep$value"
          }
          .mkString("{", ",", after)
      }
    }

    str
  }

  def escapeStr(s: String): String = s.flatMap { c =>
    val code = c.toInt

    if (code > 31 && code < 127 /* US-ASCII */ ) String.valueOf(c)
    else {
      def hexCode = code.toHexString.reverse.padTo(4, '0').reverse
      '\\' +: s"u${hexCode.toUpperCase}"
    }
  }
}
