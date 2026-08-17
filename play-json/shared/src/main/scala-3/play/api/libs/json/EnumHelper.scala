/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import scala.deriving.Mirror
import scala.quoted.{ Expr, Quotes, Type }
import scala.reflect.Enum

/**
 * Helper about [[https://dotty.epfl.ch/docs/reference/enums/enums.html Enumerations]] string representations.
 *
 * (Inspired by [[https://github.com/ReactiveMongo/ReactiveMongo-BSON/blob/master/api/src/main/scala-3/EnumHelper.scala ReactiveMongo-BSON]])
 */
object EnumHelper {

  /**
   * {{{
   * enum Color:
   *   case Red, Green, Blue
   *
   * import play.api.libs.json.EnumHelper
   *
   * val valueOf: String => Option[Color] = EnumHelper.strictValueOf[Color]
   *
   * assert(valueOf("Red") contains Color.Red)
   * assert(valueOf("red").isEmpty)
   * }}}
   */
  inline def strictValueOf[T <: Enum](using
      Mirror.SumOf[T]
  ): String => Option[T] = ${ strictValueOfImpl[T] }

  private def strictValueOfImpl[T <: Enum](using
      Quotes,
      Type[T]
  ): Expr[String => Option[T]] = enumValueOfImpl(identity, None)

  /**
   * {{{
   * enum Color:
   *   case Red, Green, Blue
   *
   * import play.api.libs.json.EnumHelper
   *
   * val valueOf: String => Option[Color] = EnumHelper.lowerCaseValueOf[Color]
   *
   * assert(valueOf("Red").isEmpty)
   * assert(valueOf("red") contains Color.Red)
   * }}}
   */
  inline def lowerCaseValueOf[T <: Enum](using
      Mirror.SumOf[T]
  ): String => Option[T] = ${ lowerCaseValueOfImpl[T] }

  private def lowerCaseValueOfImpl[T <: Enum](using
      Quotes,
      Type[T]
  ): Expr[String => Option[T]] =
    enumValueOfImpl((_: String).toLowerCase, None)

  /**
   * {{{
   * enum Color:
   *   case Red, Green, Blue
   *
   * import play.api.libs.json.EnumHelper.upperCaseValueOf
   *
   * val valueOf: String => Option[Color] = upperCaseValueOf[Color]
   *
   * assert(valueOf("red").isEmpty)
   * assert(valueOf("RED") contains Color.Red)
   * }}}
   */
  inline def upperCaseValueOf[T <: Enum](using
      Mirror.SumOf[T]
  ): String => Option[T] = ${ upperCaseValueOfImpl[T] }

  private def upperCaseValueOfImpl[T <: Enum](using
      Quotes,
      Type[T]
  ): Expr[String => Option[T]] =
    enumValueOfImpl((_: String).toUpperCase, None)

  /**
   * {{{
   * enum Color:
   *   case Red, Green, Blue
   *
   * import play.api.libs.json.EnumHelper
   *
   * val valueOf: String => Option[Color] = EnumHelper.insensitiveValueOf[Color]
   *
   * assert(valueOf("Red") contains Color.Red)
   * assert(valueOf("red") contains Color.Red)
   * assert(valueOf("RED") contains Color.Red)
   * }}}
   */
  inline def insensitiveValueOf[T <: Enum](using
      Mirror.SumOf[T]
  ): String => Option[T] = ${ insensitiveValueOfImpl[T] }

  private def insensitiveValueOfImpl[T <: Enum](using
      Quotes,
      Type[T]
  ): Expr[String => Option[T]] =
    enumValueOfImpl((_: String).toLowerCase, Some('{ (_: String).toLowerCase }))

  private def enumValueOfImpl[T <: Enum](
      labelNaming: String => String,
      normalize: Option[Expr[String => String]]
  )(using
      q: Quotes,
      tpe: Type[T]
  ): Expr[String => Option[T]] = {
    import q.reflect.*

    val tpr = TypeRepr.of(using tpe)

    val compSym = tpr.typeSymbol.companionModule

    if compSym == Symbol.noSymbol then {
      report.errorAndAbort(s"Unresolved type: ${tpr.typeSymbol.fullName}")
    }

    val compRef = Ref(compSym)

    val cases = compSym.fieldMembers
      .flatMap { fieldSym =>
        val fieldTerm = compRef.select(fieldSym)

        if fieldTerm.tpe <:< tpr then {
          Seq(fieldSym -> fieldTerm.asExprOf[T])
        } else {
          Seq.empty[(Symbol, Expr[T])]
        }
      }
      .zipWithIndex
      .map { case ((sym, expr), i) =>
        val body: Expr[Some[T]] = '{ Some(${ expr }) }

        CaseDef(
          Literal(StringConstant(labelNaming(sym.name))),
          guard = None,
          rhs = body.asTerm
        )
      }

    val none = CaseDef(
      Wildcard(),
      None,
      '{ Option.empty[T] }.asTerm
    )

    def mtch(s: Expr[String]): Expr[Option[T]] = {
      Match(s.asTerm, cases :+ none).asExprOf[Option[T]]
    }

    normalize match {
      case Some(nz) =>
        '{ (s: String) =>
          val in = ${ nz }(s)

          ${ mtch('in) }
        }

      case _ =>
        '{ (s: String) => ${ mtch('s) } }
    }
  }
}
