/*
 * Copyright (C) 2009-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import scala.quoted._

import play.api.libs.functional.ContravariantFunctor

/**
 * Implementation for the JSON macro.
 */
object JsMacroImpl {
  import Json.MacroOptions

  def withOptionsReads[A: Type, Opts <: MacroOptions: Type](
      configExpr: Expr[JsonConfiguration.Aux[Opts]]
  )(using Quotes, Type[Reads]): Expr[Reads[A]] =
    readsImpl(configExpr)

  def withOptionsWrites[A: Type, Opts <: MacroOptions: Type](
      configExpr: Expr[JsonConfiguration.Aux[Opts]]
  )(using Quotes, Type[OWrites]): Expr[OWrites[A]] =
    macroImpl[A, Opts, OWrites, Writes](configExpr, "write", "contramap", reads = false, writes = true)

  def withOptionsFormat[A: Type, Opts <: MacroOptions: Type](
      configExpr: Expr[JsonConfiguration.Aux[Opts]]
  )(using Quotes, Type[OFormat], Type[Format]): Expr[OFormat[A]] =
    withSummonedConfig(macroImpl[A, MacroOptions, OFormat, Format](_, "format", "inmap", reads = true, writes = true))

  def reads[A: Type](using Quotes, Type[Reads]): Expr[Reads[A]] =
    withSummonedConfig(readsImpl(_))

  def writes[A: Type](using Quotes, Type[OWrites]): Expr[OWrites[A]] = withSummonedConfig(
    macroImpl[A, MacroOptions, OWrites, Writes](_, "write", "contramap", reads = false, writes = true)
  )

  def format[A: Type](using Quotes, Type[Format]): Expr[OFormat[A]] = withSummonedConfig(
    macroImpl[A, MacroOptions, OFormat, Format](_, "format", "inmap", reads = true, writes = true)
  )

  // ---

  private def withSummonedConfig[T](f: Expr[JsonConfiguration.Aux[MacroOptions]] => Expr[T])(using q: Quotes): Expr[T] =
    Expr.summon[JsonConfiguration.Aux[MacroOptions]] match {
      case Some(config) =>
        f(config)

      case None =>
        q.reflect.report.errorAndAbort("No instance of JsonConfiguration is available in the implicit scope")
    }

  private def readsImpl[A: Type, OptsT <: MacroOptions: Type](
      config: Expr[JsonConfiguration],
  )(using q: Quotes): Expr[Reads[A]] = {
    import q.reflect.*

    val repr = TypeRepr.of[A]

    val helper = new QuotesHelper with OptionSupport with ImplicitResolver[A] {
      type Q = q.type
      val quotes = q

      val aTpeRepr = repr

      type Opts = OptsT
      val optsTpe = Type.of[Opts]
    }

    def singletonReader: Expr[Reads[A]] =
      Expr.summon[ValueOf[A]] match {
        case Some(vof) =>
          '{ Reads[A](_ => JsSuccess(${ vof }.value)) }

        case _ =>
          report.errorAndAbort(
            s"Something weird is going on with '${helper.prettyType(repr)}'. Should be a singleton but can't parse it"
          )
      }

    helper.knownSubclasses(repr) match {
      case Some(subTypes) =>
        ???

      case _ =>
        if (repr.isSingleton) {
          singletonReader
        } else if (repr.typeSymbol == repr.typeSymbol.moduleClass) {
          val instance = Ref(repr.typeSymbol.companionModule).asExprOf[A]

          '{ Reads[A](_ => JsSuccess($instance)) }
        } else {
          ??? // macroCaseImpl
        }
    }
  }

  private def macroImpl[A: Type, OptsT <: MacroOptions: Type, M[_]: Type, N[_]: Type](
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
}
