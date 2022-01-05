/*
 * Copyright (C) 2009-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import scala.util.{ Failure => TryFailure, Success => TrySuccess }
import scala.collection.mutable.{ Builder => MBuilder }

import scala.deriving.Mirror.ProductOf

import scala.quoted._

import play.api.libs.functional.ContravariantFunctor

/**
 * Implementation for the JSON macro.
 */
object JsMacroImpl {
  import Json.MacroOptions

  def withOptionsReads[A: Type, Opts <: MacroOptions: Type](
      configExpr: Expr[JsonConfiguration.Aux[Opts]]
  )(using
      Quotes,
      Type[Reads]
  ): Expr[Reads[A]] =
    configuredReads[A, Opts](configExpr)

  def withOptionsWrites[A: Type, Opts <: MacroOptions: Type](
      configExpr: Expr[JsonConfiguration.Aux[Opts]]
  )(using
      Quotes,
      Type[OWrites]
  ): Expr[OWrites[A]] =
    configuredWrites[A, Opts](configExpr)

  def withOptionsFormat[A: Type, Opts <: MacroOptions: Type](
      configExpr: Expr[JsonConfiguration.Aux[Opts]]
  )(using
      Quotes,
      Type[OFormat],
      Type[Format]
  ): Expr[OFormat[A]] =
    formatImpl(configExpr)

  def reads[A: Type](using
      Quotes,
      Type[Reads]
  ): Expr[Reads[A]] =
    withSummonedConfig(configuredReads(_))

  def writes[A: Type](using
      Quotes,
      Type[OWrites]
  ): Expr[OWrites[A]] =
    withSummonedConfig(configuredWrites(_))

  def format[A: Type](using
      Quotes,
      Type[Format]
  ): Expr[OFormat[A]] =
    withSummonedConfig(formatImpl(_))

  def anyValFormat[A <: AnyVal: Type](using
      Quotes
  ): Expr[Format[A]] = '{
    Format[A](${ anyValReads[A] }, ${ anyValWrites[A] })
  }

  // ---

  private def withSummonedConfig[T](f: Expr[JsonConfiguration.Aux[MacroOptions]] => Expr[T])(using
      q: Quotes
  ): Expr[T] =
    Expr.summon[JsonConfiguration.Aux[MacroOptions]] match {
      case Some(config) =>
        f(config)

      case None =>
        q.reflect.report.errorAndAbort("No instance of JsonConfiguration is available in the implicit scope")
    }

  inline private def withSelfOReads[A](
      f: Reads[A] => Reads[A]
  ): Reads[A] = {
    new Reads[A] { self =>
      lazy val underlying = f(self)

      def reads(js: JsValue): JsResult[A] = underlying.reads(js)
    }
  }

  private def configuredReads[A: Type, OptsT <: MacroOptions: Type](
      config: Expr[JsonConfiguration],
  )(using
      q: Quotes
  ): Expr[Reads[A]] = {
    import q.reflect.*

    '{
      lazy val cfg = ${ config }

      withSelfOReads[A] { (forwardReads: Reads[A]) => ${ readsExpr[A, OptsT]('cfg, 'forwardReads) } }
    }
  }

  private def readsExpr[A: Type, OptsT <: MacroOptions: Type](
      config: Expr[JsonConfiguration],
      forwardExpr: Expr[Reads[A]]
  )(using
      q: Quotes,
      rt: Type[Reads]
  ): Expr[Reads[A]] = {
    import q.reflect.*

    val tpe = Type.of[A]
    val repr = TypeRepr.of[A](using
      tpe
    )

    val helper = new ReadsHelper[q.type, A] {
      type Q = q.type
      val quotes = q

      val aTpe = tpe

      type Opts = OptsT
      val optsTpe = Type.of[Opts]

      val readsTpe = rt
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
        helper.familyReads(config, forwardExpr, subTypes)

      case _ => {
        import helper.{ prettyType, productReads }

        if (repr.isSingleton) {
          singletonReader
        } else if (repr.typeSymbol == repr.typeSymbol.moduleClass) {
          val instance = Ref(repr.typeSymbol.companionModule).asExprOf[A]

          '{ Reads[A](_ => JsSuccess($instance)) }
        } else {
          tpe match {
            case '[IsProduct[t]] =>
              Expr.summon[ProductOf[t]] match {
                case Some(pof) =>
                  productReads[t, t](config, forwardExpr.asExprOf[Reads[t]], pof).asExprOf[Reads[A]]

                case _ =>
                  report.errorAndAbort(
                    s"Instance not found: 'ProductOf[${prettyType(repr)}]'"
                  )
              }

            case '[t] =>
              Expr.summon[Conversion[t, _ <: Product]] match {
                case Some('{ $conv: Conversion[t, IsProduct[p]] }) =>
                  Expr.summon[ProductOf[t]] match {
                    case Some(pof) =>
                      productReads[t, p](config, forwardExpr.asExprOf[Reads[t]], pof).asExprOf[Reads[A]]

                    case _ =>
                      report.errorAndAbort(
                        s"Instance not found: 'ProductOf[${prettyType(repr)}]'"
                      )
                  }

                case _ =>
                  report.errorAndAbort(s"Instance not found: 'Conversion[${prettyType(repr)}, _ <: Product]'")
              }
          }
        }
      }
    }
  }

  private sealed trait ReadsHelper[Qts <: Quotes, A] extends QuotesHelper with OptionSupport with ImplicitResolver[A] {
    type Q = Qts
    val quotes: Q

    protected val readsTpe: Type[Reads]

    given _q: Quotes = quotes
    import quotes.reflect.*

    def familyReads(
        config: Expr[JsonConfiguration],
        forwardExpr: Expr[Reads[A]],
        subTypes: List[TypeRepr]
    ): Expr[Reads[A]] = {
      def handleSubTypes(discriminator: Expr[String], input: Expr[JsValue]): Expr[JsResult[A]] = {
        type Subtype[U <: A] = U

        val cases = subTypes.zipWithIndex.map { (tpr, i) =>
          tpr.asType match {
            case st @ '[Subtype[sub]] =>
              val subTpr = TypeRepr.of[sub](using
                st
              )

              val bind = Symbol.newBind(
                Symbol.spliceOwner,
                s"macroTpe${i}",
                Flags.Case,
                TypeRepr.of[String]
              )

              val tpeCaseName: Expr[String] = '{
                ${ config }.typeNaming(${ Expr(subTpr.typeSymbol.fullName) })
              }

              val resolve = resolver[Reads, sub](
                forwardExpr.asExprOf[Reads[sub]],
                debug
              )(readsTpe)

              val body: Expr[JsResult[sub]] = resolve(subTpr) match {
                case Some((givenReads, _)) =>
                  '{
                    ${ givenReads.asExprOf[Reads[sub]] }.reads(${ input })
                  }

                case _ =>
                  report.errorAndAbort(s"Instance not found: ${classOf[Reads[_]].getName}[${prettyType(tpr)}]")
              }

              val matchedRef: Expr[String] = Ref(bind).asExprOf[String]

              val cond: Expr[Boolean] = '{ ${ matchedRef } == ${ tpeCaseName } }

              CaseDef(Bind(bind, Wildcard()), guard = Some(cond.asTerm), rhs = body.asTerm)
          }
        }

        val fallback = CaseDef(Wildcard(), None, '{ JsError("error.invalid") }.asTerm)

        Match(discriminator.asTerm, cases :+ fallback).asExprOf[JsResult[A]]
      }

      '{
        Reads[A] {
          case obj @ JsObject(_) =>
            obj.value.get(${ config }.discriminator) match {
              case Some(jsDiscriminator) => {
                // Either read the whole object, or a nested _value object
                lazy val input = obj.value.get("_value").getOrElse(obj)

                jsDiscriminator.validate[String].flatMap { discriminator =>
                  ${ handleSubTypes('discriminator, 'input) }
                }
              }

              case _ =>
                JsError(JsPath \ ${ config }.discriminator, "error.missing.path")
            }

          case _ => JsError("error.expected.jsobject")
        }
      }
    }

    private case class ReadableField[T](sym: Symbol, i: Int, tpr: TypeRepr, default: Option[Expr[T]])

    def productReads[T: Type, P <: Product: Type](
        config: Expr[JsonConfiguration],
        forwardExpr: Expr[Reads[T]],
        pof: Expr[ProductOf[T]]
    ): Expr[Reads[T]] = {
      val tpr = TypeRepr.of[T]
      val tprElements = productElements[T, P](tpr, pof) match {
        case TryFailure(cause) =>
          report.errorAndAbort(cause.getMessage)

        case TrySuccess(elms) =>
          elms
      }

      val types   = tprElements.map(_._2)
      val resolve = resolver[Reads, T](forwardExpr, debug)(readsTpe)
      val compCls = tpr.typeSymbol.companionClass
      val compMod = Ref(tpr.typeSymbol.companionModule)

      val (optional, required) = tprElements.zipWithIndex.map {
        case ((sym, pt), i) =>
          pt.asType match {
            case '[t] =>
              val default: Option[Expr[t]] =
                compCls.declaredMethod(f"$$lessinit$$greater$$default$$" + (i + 1)).headOption.collect {
                  case defaultSym if sym.flags.is(Flags.HasDefault) =>
                    compMod.select(defaultSym).asExprOf[t]
                }
              val _: TypeRepr = pt

              ReadableField(sym, i, pt, default)
          }
      }.toSeq.partition { case ReadableField(_, _, t, _) => isOptionalType(t) }

      type JsFailure    = (JsPath, Seq[JsonValidationError])
      type ExceptionAcc = MBuilder[JsFailure, Seq[JsFailure]]

      def withIdents[U: Type](
          f: Expr[ExceptionAcc] => Expr[U]
      ): Expr[U] = '{
        val err = Seq.newBuilder[JsFailure]

        ${ f('{ err }) }
      }

      // For required field
      def tryWithDefault[U: Type](
          res: Expr[JsResult[U]],
          default: Expr[U]
      ): Expr[JsResult[U]] =
        '{
          ${ res } match {
            case JsError(_) =>
              JsSuccess(${ default })

            case result =>
              result
          }
        }

      // For optional field
      def tryWithOptDefault[U: Type](
          res: Expr[JsResult[Option[U]]],
          default: Expr[Option[U]]
      ): Expr[JsResult[Option[U]]] =
        '{
          ${ res } match {
            case JsError(_) | JsSuccess(None, _) =>
              JsSuccess(${ default })

            case result =>
              result
          }
        }

      /* TODO
            (isOption, defaultValue) match {
              case (true, Some(v)) =>
                val c = TermName(s"${methodName}HandlerWithDefault")
                q"$config.optionHandlers.$c($jspathTree, $v)($impl)"

              case (true, _) =>
                val c = TermName(s"${methodName}Handler")
                q"$config.optionHandlers.$c($jspathTree)($impl)"

              case (false, Some(v)) =>
                val c = TermName(s"${methodName}WithDefault")
                q"$jspathTree.$c($v)($impl)"

              case _ =>
                q"$jspathTree.${TermName(methodName)}($impl)"
            }
       */

      ???
    }
  }

  inline private def withSelfOWrites[A](
      f: OWrites[A] => OWrites[A]
  ): OWrites[A] = {
    new OWrites[A] { self =>
      lazy val underlying = f(self)

      def writes(a: A): JsObject = underlying.writes(a)
    }
  }

  private def configuredWrites[A: Type, OptsT <: MacroOptions: Type](
      config: Expr[JsonConfiguration],
  )(using
      q: Quotes
  ): Expr[OWrites[A]] = {
    import q.reflect.*

    ensureFindType[A]

    '{
      lazy val cfg = ${ config }

      withSelfOWrites[A] { (forwardWrites: OWrites[A]) => ${ writesExpr[A, OptsT]('cfg, 'forwardWrites) } }
    }
  }

  private def writesExpr[A: Type, OptsT <: MacroOptions: Type](
      config: Expr[JsonConfiguration],
      forwardExpr: Expr[OWrites[A]]
  )(using
      q: Quotes,
      wt: Type[Writes]
  ): Expr[OWrites[A]] = {
    import q.reflect.*

    val tpe = Type.of[A]
    val repr = TypeRepr.of[A](using
      tpe
    )

    val helper = new WritesHelper[q.type, A] {
      type Q = q.type
      val quotes = q

      val aTpe = tpe

      type Opts = OptsT
      val optsTpe = Type.of[Opts]

      val writesTpe = wt
    }

    import helper.{ prettyType, productWrites }

    helper.knownSubclasses(repr) match {
      case Some(subTypes) =>
        helper.familyWrites(config, forwardExpr, subTypes)

      case _ =>
        if (
          repr.isSingleton ||
          repr.typeSymbol == repr.typeSymbol.moduleClass
        ) {
          '{
            val empty = Json.obj()
            OWrites[A](_ => empty)
          }
        } else {
          tpe match {
            case '[IsProduct[t]] =>
              Expr.summon[ProductOf[t]] match {
                case Some(pof) =>
                  productWrites[t, t](config, forwardExpr.asExprOf[OWrites[t]], '{ identity[t] }, pof)
                    .asExprOf[OWrites[A]]

                case _ =>
                  report.errorAndAbort(
                    s"Instance not found: 'ProductOf[${prettyType(repr)}]'"
                  )
              }

            case '[t] =>
              Expr.summon[Conversion[t, _ <: Product]] match {
                case Some('{ $conv: Conversion[t, IsProduct[p]] }) =>
                  Expr.summon[ProductOf[t]] match {
                    case Some(pof) =>
                      productWrites[t, p](config, forwardExpr.asExprOf[OWrites[t]], conv, pof).asExprOf[OWrites[A]]

                    case _ =>
                      report.errorAndAbort(
                        s"Instance not found: 'ProductOf[${prettyType(repr)}]'"
                      )
                  }

                case _ =>
                  report.errorAndAbort(s"Instance not found: 'Conversion[${prettyType(repr)}, _ <: Product]'")
              }
          }
        }
    }
  }

  private sealed trait WritesHelper[Qts <: Quotes, A] extends QuotesHelper with OptionSupport with ImplicitResolver[A] {
    type Q = Qts
    val quotes: Q

    protected val writesTpe: Type[Writes]

    given _q: Quotes = quotes
    import quotes.reflect.*

    def familyWrites(
        config: Expr[JsonConfiguration],
        forwardExpr: Expr[OWrites[A]],
        subTypes: List[TypeRepr]
    ): Expr[OWrites[A]] = {
      def handleSubTypes(input: Expr[A]): Expr[JsObject] = {
        type Subtype[U <: A] = U

        val cases = subTypes.zipWithIndex.map { (tpr, i) =>
          tpr.asType match {
            case st @ '[Subtype[sub]] =>
              val subTpr = TypeRepr.of[sub](using
                st
              )

              val bind = Symbol.newBind(
                Symbol.spliceOwner,
                s"macroVal${i}",
                Flags.Case,
                subTpr
              )

              val tpeCaseName: Expr[String] = '{
                ${ config }.typeNaming(${ Expr(subTpr.typeSymbol.fullName) })
              }

              val resolve = resolver[Writes, sub](
                forwardExpr.asExprOf[Writes[sub]],
                debug
              )(writesTpe)

              val matchedRef: Expr[sub] = Ref(bind).asExprOf[sub]

              val body: Expr[JsObject] = resolve(subTpr) match {
                case Some((givenWrites, _)) =>
                  '{
                    def output: JsObject = ${ givenWrites.asExprOf[Writes[sub]] }.writes($matchedRef) match {
                      case obj @ JsObject(_) =>
                        obj

                      case jsValue =>
                        Json.obj("_value" -> jsValue)
                    }

                    output ++ JsObject(Map(${ config }.discriminator -> JsString(${ config }.typeNaming(${
                      tpeCaseName
                    }))))
                  }

                case _ =>
                  report.errorAndAbort(s"Instance not found: ${classOf[Writes[_]].getName}[${prettyType(tpr)}]")

              }

              CaseDef(Bind(bind, Typed(Wildcard(), Inferred(tpr))), guard = None, rhs = body.asTerm)
          }
        }

        Match(input.asTerm, cases).asExprOf[JsObject]
      }

      '{
        OWrites[A] { (a: A) => ${ handleSubTypes('a) } }
      }
    }

    def productWrites[T: Type, P <: Product: Type](
        config: Expr[JsonConfiguration],
        forwardExpr: Expr[OWrites[T]],
        toProduct: Expr[T => P],
        pof: Expr[ProductOf[T]]
    ): Expr[OWrites[T]] = {
      val tpr = TypeRepr.of[T]

      ???
    }
  }

  private def formatImpl[A: Type, Opts <: MacroOptions: Type](
      config: Expr[JsonConfiguration],
  )(using
      Quotes
  ): Expr[OFormat[A]] = '{
    val reads  = ${ configuredReads[A, Opts](config) }
    val writes = ${ configuredWrites[A, Opts](config) }

    OFormat[A](reads, writes)
  }

  def anyValReads[A <: AnyVal: Type](using
      q: Quotes
  ): Expr[Reads[A]] = {
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
                      New(Inferred(aTpr)).select(ctor).appliedTo(in.asTerm).asExprOf[A]

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

  def anyValWrites[A <: AnyVal: Type](using
      q: Quotes
  ): Expr[Writes[A]] = {
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
                        .asExprOf[t](using
                          vtpe
                        )
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

  // ---

  private def ensureFindType[A](using
      q: Quotes,
      tpe: Type[A]
  ): Unit = {
    import q.reflect.*

    TypeRepr
      .of[A](using
        tpe
      )
      .dealias match {
      case OrType(_, _) =>

      case notFound if notFound.typeSymbol == Symbol.noSymbol =>
        report.errorAndAbort("Type not found")

      case _ =>
    }
  }

  private type IsProduct[U <: Product] = U

  private def debug(msg: => String)(using
      q: Quotes
  ): Unit =
    if (debugEnabled) q.reflect.report.info(msg)

  private lazy val debugEnabled: Boolean =
    Option(System.getProperty("play.json.macro.debug")).filterNot(_.isEmpty).map(_.toLowerCase).exists { v =>
      "true".equals(v) || v.substring(0, 1) == "y"
    }
}
