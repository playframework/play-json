/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import java.lang.Enum as JEnum

private[json] trait EnvValueMacros { self: JsValueMacros =>
  inline def javaEnumReads[T <: JEnum[_]]: Reads[T] = ${ EnvValueMacrosImpl.reads[T] }

  inline def javaEnumFormat[T <: JEnum[_]]: Format[T] =
    Format[T](javaEnumReads[T], summon[Writes[T]])
}

private[json] object EnvValueMacrosImpl {
  import scala.quoted.*

  def reads[T <: JEnum[_]: Type](using q: Quotes): Expr[Reads[T]] = {
    import q.reflect.*

    val tpr     = TypeRepr.of[T]
    val compCls = tpr.typeSymbol.companionClass

    type IsEnum[E <: T] = E

    val cases = compCls.declaredFields.flatMap { field =>
      field.typeRef.asType match {
        case '[IsEnum[e]] => {
          val constName = Expr(field.name)
          val const     =
            Select(
              Ref(compCls.companionModule),
              field
            ).asExprOf[e]

          Some(
            CaseDef(
              Literal(StringConstant(field.name)),
              guard = None,
              rhs = '{ JsSuccess[T]($const) }.asTerm
            )
          )
        }

        case _ =>
          None
      }
    }

    val errKey = '{ "error.invalid.enum." + ${ Expr(tpr.typeSymbol.name) } }

    val fallback = CaseDef(Wildcard(), None, '{ JsError(${ errKey }) }.asTerm)

    def mt(constName: Expr[String]) =
      Match(constName.asTerm, cases :+ fallback).asExprOf[JsResult[T]]

    '{
      Reads.StringReads.flatMapResult[T] { str =>
        ${ mt('str) }
      }
    }
  }
}
