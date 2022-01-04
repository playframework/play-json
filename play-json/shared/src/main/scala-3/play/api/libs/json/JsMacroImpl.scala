/*
 * Copyright (C) 2009-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import scala.deriving._

import scala.quoted._

import play.api.libs.functional.ContravariantFunctor

/**
 * Implementation for the JSON macro.
 */
object JsMacroImpl {
  def withOptionsReads[A: Type](configExpr: Expr[JsonConfiguration])(using Quotes, Type[Reads]): Expr[Reads[A]] =
    macroImpl[A, Reads, Reads](configExpr, "read", "map", reads = true, writes = false)

  def withOptionsWrites[A: Type](configExpr: Expr[JsonConfiguration])(using Quotes, Type[OWrites]): Expr[OWrites[A]] =
    macroImpl[A, OWrites, Writes](configExpr, "write", "contramap", reads = false, writes = true)

  def withOptionsFormat[A: Type](
      configExpr: Expr[JsonConfiguration]
  )(using Quotes, Type[OFormat], Type[Format]): Expr[OFormat[A]] =
    withSummonedConfig(macroImpl[A, OFormat, Format](_, "format", "inmap", reads = true, writes = true))

  def implicitConfigReads[A: Type](using Quotes, Type[Reads]): Expr[Reads[A]] =
    withSummonedConfig(macroImpl[A, Reads, Reads](_, "read", "map", reads = true, writes = false))

  def implicitConfigWrites[A: Type](using Quotes, Type[OWrites]): Expr[OWrites[A]] = withSummonedConfig(
    macroImpl[A, OWrites, Writes](_, "write", "contramap", reads = false, writes = true)
  )

  def implicitConfigFormat[A: Type](using Quotes, Type[Format]): Expr[OFormat[A]] = withSummonedConfig(
    macroImpl[A, OFormat, Format](_, "format", "inmap", reads = true, writes = true)
  )

  // ---

  private def withSummonedConfig[T](f: Expr[JsonConfiguration] => Expr[T])(using q: Quotes): Expr[T] =
    Expr.summon[JsonConfiguration] match {
      case Some(config) =>
        f(config)

      case None =>
        q.reflect.report.errorAndAbort("No instance of JsonConfiguration is available in the implicit scope")
    }

  /**
   * Generic implementation of the macro.
   *
   * The reads/writes flags are used to say whether a reads/writes is being generated (in the case format,
   * these are both true).  This is also used to determine what arguments should be passed to various
   * functions, for example, if the apply, unapply, or both should be passed to the functional builder apply
   * method.
   *
   * @param config The configuration tree
   * @param methodName The name of the method on JsPath that gets called, ie, read/write/format
   * @param mapLikeMethod The method that's used to map the type of thing being built, used in case there is
   * only one field in the case class.
   * @param reads Whether we should generate a reads.
   * @param writes Whether we should generate a writes
   * @param atag The class of the type we're generating a reads/writes/format for.
   * @param matag The class of the reads/writes/format.
   * @param natag The class of the reads/writes/format.
   */
  private def macroImpl[A: Type, M[_]: Type, N[_]: Type](
      config: Expr[JsonConfiguration],
      methodName: String,
      mapLikeMethod: String,
      reads: Boolean,
      writes: Boolean
  )(using q: Quotes): Expr[M[A]] = ???

  def anyValReads[A <: AnyVal: Type](using q: Quotes): Expr[Reads[A]] = {
    import q.reflect.*

    val aTpr = TypeRepr.of[A]
    val ctor = aTpr.typeSymbol.primaryConstructor

    ctor.paramSymss match {
      case List(v: Symbol) :: Nil =>
        v.tree match {
          case vd: ValDef => {
            val tpr = vd.tpt.tpe

            tpr.asType match {
              case vtpe @ '[t] =>
                Expr.summon[Reads[t]] match {
                  case Some(reads) => {
                    def mapf(in: Expr[t]): Expr[A] =
                      New(Inferred(aTpr))
                        .select(ctor)
                        .appliedTo(in.asTerm)
                        .asExprOf[A]

                    val expr = '{
                      ${ reads }.map { v => ${ mapf('v) } }
                    }

                    debug(expr.show)

                    expr
                  }

                  case None =>
                    report.errorAndAbort(
                      s"Instance not found: ${classOf[Reads[_]].getName}[${tpr.typeSymbol.fullName}]"
                    )
                }
            }
          }

          case _ =>
            report.errorAndAbort(
              s"Constructor parameter expected, found: ${v}"
            )
        }

      case _ =>
        report.errorAndAbort(
          s"Cannot resolve value reader for '${aTpr.typeSymbol.name}'"
        )

    }
  }

  def anyValWrites[A <: AnyVal: Type](using q: Quotes): Expr[Writes[A]] = {
    import q.reflect.*

    val aTpr = TypeRepr.of[A]
    val ctor = aTpr.typeSymbol.primaryConstructor

    ctor.paramSymss match {
      case List(v: Symbol) :: Nil =>
        v.tree match {
          case vd: ValDef => {
            val tpr = vd.tpt.tpe

            tpr.asType match {
              case vtpe @ '[t] =>
                Expr.summon[Writes[t]] match {
                  case Some(writes) => {
                    def contramapf(in: Expr[A]): Expr[t] = {
                      val term = in.asTerm

                      term
                        .select(term.symbol.fieldMember(v.name))
                        .asExprOf[t](using vtpe)
                    }

                    val expr = '{
                      val fn = summon[ContravariantFunctor[Writes]]

                      fn.contramap[t, A](${ writes }, (in: A) => ${ contramapf('in) })
                    }

                    debug(expr.show)

                    expr
                  }

                  case None =>
                    report.errorAndAbort(
                      s"Instance not found: ${classOf[Writes[_]].getName}[${tpr.typeSymbol.fullName}]"
                    )
                }
            }
          }

          case _ =>
            report.errorAndAbort(
              s"Constructor parameter expected, found: ${v}"
            )
        }

      case _ =>
        report.errorAndAbort(
          s"Cannot resolve value writer for '${aTpr.typeSymbol.name}'"
        )

    }
  }

  def anyValFormat[A <: AnyVal: Type](using Quotes): Expr[Format[A]] = '{
    Format[A](${ anyValReads[A] }, ${ anyValWrites[A] })
  }

  private def debug(msg: => String)(using q: Quotes): Unit =
    if (debugEnabled) q.reflect.report.info(msg)

  private lazy val debugEnabled: Boolean =
    Option(System.getProperty("play.json.macro.debug")).filterNot(_.isEmpty).map(_.toLowerCase).exists { v =>
      "true".equals(v) || v.substring(0, 1) == "y"
    }

  // ---

  /** Only for internal purposes */
  final class Placeholder {} // TODO: Common with Scala-2

  /** Only for internal purposes */
  object Placeholder {
    implicit object Format extends OFormat[Placeholder] {
      val success                                     = JsSuccess(new Placeholder())
      def reads(json: JsValue): JsResult[Placeholder] = success
      def writes(pl: Placeholder)                     = Json.obj()
    }
  }
}
