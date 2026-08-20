/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import java.lang.{ Enum => JEnum }

import scala.reflect.macros.blackbox

/**
 * Helper about Java Enum string representations.
 */
final class JavaEnumHandlerImpl(val c: blackbox.Context) {
  import c.universe._

  def reads[T <: JEnum[?]: c.WeakTypeTag]: c.Expr[Reads[T]] = {
    val tag       = implicitly[c.WeakTypeTag[T]]
    val tpe       = tag.tpe.dealias
    val sym       = tpe.typeSymbol
    val companion = sym.companion
    val libs      = q"_root_.play.api.libs"
    val json      = q"$libs.json"

    val constants = companion.info.decls.collect {
      case d if d.typeSignature <:< tpe =>
        val nme = d.name.toString
        val lit = q"${nme}"
        val tn  = TermName(nme)

        cq"${lit} => $json.JsSuccess($companion.$tn)"
    }.toList

    val errKey = q""""error.invalid.enum." + ${sym.name.toString}"""

    val strMatch = Match(q"s", constants :+ cq"""_ => $json.JsError($errKey)""")

    val generated = q"""implicitly[$json.Reads[String]].flatMapResult[$tpe] { s =>
      $strMatch
    }"""

    debug(showCode(generated))

    c.Expr[Reads[T]](c.typeCheck(generated))
  }

  def format[T <: JEnum[?]: c.WeakTypeTag]: c.Expr[Format[T]] = {
    val tag  = implicitly[c.WeakTypeTag[T]]
    val tpe  = tag.tpe.dealias
    val libs = q"_root_.play.api.libs"
    val json = q"$libs.json"

    val generated = q"$json.Format[$tpe](${reads[T]}, implicitly[$json.Writes[$tpe]])"

    debug(showCode(generated))

    c.Expr[Format[T]](c.typeCheck(generated))
  }

  // ---

  private def debug(msg: => String): Unit = {
    if (debugEnabled) {
      c.info(c.enclosingPosition, msg, force = false)
    }
  }

  private lazy val debugEnabled =
    Option(System.getProperty("play.json.macro.debug")).filterNot(_.isEmpty).map(_.toLowerCase).exists { v =>
      "true".equals(v) || v.substring(0, 1) == "y"
    }
}
