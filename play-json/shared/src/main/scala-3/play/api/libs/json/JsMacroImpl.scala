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
object JsMacroImpl { // TODO: debug
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

  private def withSummonedConfig[T: Type](f: Expr[JsonConfiguration.Aux[_ <: MacroOptions]] => Expr[T])(using
      q: Quotes
  ): Expr[T] =
    Expr.summon[JsonConfiguration.Aux[_ <: MacroOptions]] match {
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

    val expr = '{
      lazy val cfg = ${ config }

      withSelfOReads[A] { (forwardReads: Reads[A]) => ${ readsExpr[A, OptsT]('cfg, 'forwardReads) } }
    }

    if (debugEnabled) {
      report.info(s"/* Generated Reads:\n${expr.asTerm.show(using
        Printer.TreeAnsiCode
      )}\n*/")
    }

    expr
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

  private sealed trait ReadsHelper[Qts <: Quotes, A]
      extends MacroHelpers
      with QuotesHelper
      with OptionSupport
      with ImplicitResolver[A] {
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

        val cases = subTypes.filter {
          case tpr @ AppliedType(_, _) => {
            report.warning(
              s"Generic type ${prettyType(tpr)} is not supported as member of sealed family ${prettyType(aTpeRepr)}."
            )

            false
          }

          case _ => true
        }.zipWithIndex.map { (tpr, i) =>
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
                ${ config }.typeNaming(${ Expr(typeName(tpr.typeSymbol)) })
              }

              val resolve = resolver[Reads, sub](
                '{
                  @SuppressWarnings(Array("AsInstanceOf"))
                  def forward =
                    ${ forwardExpr }.asInstanceOf[Reads[sub]]

                  forward
                },
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
              val cond: Expr[Boolean]      = '{ ${ matchedRef } == ${ tpeCaseName } }

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
        case ((sym, rpt), i) =>
          val pt = rpt.dealias

          pt.asType match {
            case '[t] =>
              val default: Option[Expr[t]] =
                compCls.declaredMethod(f"$$lessinit$$greater$$default$$" + (i + 1)).headOption.collect {
                  case defaultSym if sym.flags.is(Flags.HasDefault) =>
                    compMod.select(defaultSym).asExprOf[t]
                }

              ReadableField(sym, i, pt, default)
          }
      }.toSeq.partition { case ReadableField(_, _, t, _) => isOptionalType(t) }

      def readFields(input: Expr[JsObject]): Expr[JsResult[T]] = {
        val reqElmts: Seq[(Int, Expr[JsResult[_]])] = required.map {
          case ReadableField(param, n, pt, defaultValue) =>
            pt.asType match {
              case ptpe @ '[p] =>
                val reads: Expr[Reads[p]] = resolve(pt) match {
                  case Some((givenReads, _)) =>
                    givenReads.asExprOf[Reads[p]]

                  case _ =>
                    report.errorAndAbort(s"Instance not found: ${classOf[Reads[_]].getName}[${prettyType(pt)}]")
                }

                val pname = param.name

                val get: Expr[JsResult[p]] = {
                  val field = '{ ${ config }.naming(${ Expr(pname) }) }
                  val path  = '{ JsPath \ ${ field } }

                  val pathReads: Expr[Reads[p]] = defaultValue match {
                    case Some(v) =>
                      '{ ${ path }.readWithDefault[p](${ v.asExprOf[p] })($reads) }

                    case _ =>
                      '{ ${ path }.read[p]($reads) }
                  }

                  '{ ${ pathReads }.reads($input) }
                }

                n -> get
            }
        }

        val exElmts: Seq[(Int, Expr[JsResult[_]])] = optional.map {
          case p @ ReadableField(_, _, OptionTypeParameter(it), _) =>
            p.copy(tpr = it)

          case ReadableField(param, _, pt, _) =>
            report.errorAndAbort(
              s"Invalid optional field '${param.name}': ${prettyType(pt)}"
            )

        }.map {
          case ReadableField(param, n, it, defaultValue) =>
            val pname = param.name

            it.asType match {
              case '[i] =>
                val reads: Expr[Reads[i]] = resolve(it) match {
                  case Some((givenReads, _)) =>
                    givenReads.asExprOf[Reads[i]]

                  case _ =>
                    report.errorAndAbort(s"Instance not found: ${classOf[Reads[_]].getName}[Option[${prettyType(it)}]]")
                }

                type p = Option[i]

                val get: Expr[JsResult[p]] = {
                  val field = '{ ${ config }.naming(${ Expr(pname) }) }
                  val path  = '{ JsPath \ ${ field } }

                  val pathReads: Expr[Reads[p]] = defaultValue match {
                    case Some(v) =>
                      '{ ${ config }.optionHandlers.readHandlerWithDefault($path, ${ v.asExprOf[p] })($reads) }

                    case _ =>
                      '{ ${ config }.optionHandlers.readHandler($path)($reads) }
                  }

                  '{ ${ pathReads }.reads($input) }
                }

                n -> get
            }
        }

        val tupElmts: Seq[Expr[JsResult[_]]] =
          (reqElmts ++ exElmts).toSeq.sortBy(_._1).map(_._2)

        '{
          trySeq(${ Expr.ofSeq(tupElmts) }).map { ls => ${ pof }.fromProduct(Tuple.fromArray(ls.toArray)) }
        }
      }

      '{
        Reads[T] {
          case obj @ JsObject(_) =>
            ${ readFields('obj) }

          case _ =>
            JsError("error.expected.jsobject")
        }
      }
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

    val expr = '{
      lazy val cfg = ${ config }

      withSelfOWrites[A] { (forwardWrites: OWrites[A]) => ${ writesExpr[A, OptsT]('cfg, 'forwardWrites) } }
    }

    if (debugEnabled) {
      report.info(s"/* Generated OWrites:\n${expr.asTerm.show(using
        Printer.TreeAnsiCode
      )}\n*/")
    }

    expr
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

  private sealed trait WritesHelper[Qts <: Quotes, A]
      extends MacroHelpers
      with QuotesHelper
      with OptionSupport
      with ImplicitResolver[A] {
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

        val cases = subTypes.filter {
          case tpr @ AppliedType(_, _) => {
            report.warning(
              s"Generic type ${prettyType(tpr)} is not supported as a member of sealed family ${prettyType(aTpeRepr)}"
            )

            false
          }

          case _ =>
            true
        }.zipWithIndex.map { (tpr, i) =>
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
                ${ config }.typeNaming(${ Expr(typeName(tpr.typeSymbol)) })
              }

              val resolve = resolver[Writes, sub](
                '{ ${ forwardExpr }.asInstanceOf[Writes[sub]] },
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

                    output ++ JsObject(Map(${ config }.discriminator -> JsString(${ tpeCaseName })))
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

    private case class WritableField(sym: Symbol, i: Int, pt: TypeRepr)

    def productWrites[T: Type, P <: Product: Type](
        config: Expr[JsonConfiguration],
        forwardExpr: Expr[OWrites[T]],
        toProduct: Expr[T => P],
        pof: Expr[ProductOf[T]]
    ): Expr[OWrites[T]] = {
      val tpr = TypeRepr.of[T]
      val tprElements = productElements[T, P](tpr, pof) match {
        case TryFailure(cause) =>
          report.errorAndAbort(cause.getMessage)

        case TrySuccess(elms) =>
          elms
      }

      val types   = tprElements.map(_._2)
      val resolve = resolver[Writes, T](forwardExpr, debug)(writesTpe)

      val (optional, required) = tprElements.zipWithIndex.view.map {
        case ((sym, rpt), i) =>
          val pt = rpt.dealias

          pt.asType match {
            case '[t] =>
              WritableField(sym, i, pt)
          }
      }.toSeq.partition { case WritableField(_, _, t) => isOptionalType(t) }

      type ElementAcc = MBuilder[(String, JsValue), Map[String, JsValue]]

      def withIdents[U: Type](f: Expr[ElementAcc] => Expr[U]): Expr[U] =
        '{
          val ok = Map.newBuilder[String, JsValue]

          ${ f('{ ok }) }
        }

      val (tupleTpe, withTupled) =
        withTuple[T, P, JsObject](tpr, toProduct, types)

      def writeFields(input: Expr[T]): Expr[JsObject] =
        withTupled(input) { tupled =>
          val fieldMap = withFields(tupled, tupleTpe, tprElements, debug)

          withIdents[JsObject] { bufOk =>
            val values: Seq[Expr[Unit]] = required.map {
              case WritableField(param, i, pt) =>
                val pname = param.name

                val withField = fieldMap.get(pname) match {
                  case Some(f) => f

                  case _ =>
                    report.errorAndAbort(
                      s"Field not found: ${prettyType(tpr)}.${pname}"
                    )
                }

                pt.asType match {
                  case pTpe @ '[p] =>
                    val writes: Expr[Writes[p]] = resolve(pt) match {
                      case Some((givenWrites, _)) =>
                        givenWrites.asExprOf[Writes[p]]

                      case _ =>
                        report.errorAndAbort(s"Instance not found: ${classOf[Writes[_]].getName}[${prettyType(pt)}]")
                    }

                    withField { v =>
                      ('{
                        val nme = ${ config }.naming(${ Expr(pname) })
                        ${ bufOk } += nme -> ${ writes }.writes(${ v.asExprOf[p] })
                        ()
                      }).asTerm
                    }.asExprOf[Unit]
                }
            } // end of required.map

            val extra: Seq[Expr[Unit]] = optional.map {
              case WritableField(param, i, optType @ OptionTypeParameter(pt)) =>
                val pname = param.name

                val withField = fieldMap.get(pname) match {
                  case Some(f) => f

                  case _ =>
                    report.errorAndAbort(
                      s"Field not found: ${prettyType(tpr)}.${pname}"
                    )
                }

                pt.asType match {
                  case pTpe @ '[p] =>
                    val writes: Expr[Writes[p]] = resolve(pt) match {
                      case Some((givenWrites, _)) =>
                        givenWrites.asExprOf[Writes[p]]

                      case _ =>
                        report.errorAndAbort(s"Instance not found: ${classOf[Writes[_]].getName}[${prettyType(pt)}]")
                    }

                    val field = '{ ${ config }.naming(${ Expr(pname) }) }
                    val path  = '{ JsPath \ ${ field } }

                    val pathWrites: Expr[OWrites[Option[p]]] = '{
                      ${ config }.optionHandlers.writeHandler($path)($writes)
                    }

                    withField { v =>
                      ('{
                        val nme = ${ config }.naming(${ Expr(pname) })
                        val js  = ${ pathWrites }.writes(${ v.asExprOf[Option[p]] })

                        ${ bufOk } ++= js.value
                        ()
                      }).asTerm
                    }.asExprOf[Unit]
                }
            } // end of extra.collect

            if (values.isEmpty && extra.isEmpty) {
              debug(
                s"No field found: class ${prettyType(TypeRepr.of[T])}"
              )

              '{ JsObject(Map.empty) }
            } else {
              val fields = values ++ extra

              val resExpr = '{ JsObject(${ bufOk }.result()) }

              Block(fields.map(_.asTerm).toList, resExpr.asTerm).asExprOf[JsObject]
            }
          }
        }

      '{
        OWrites[T] { (t: T) => ${ writeFields('t) } }
      }
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

  inline private def trySeq(in: Seq[JsResult[_]]): JsResult[Seq[Any]] = {
    type JsFail = (JsPath, collection.Seq[JsonValidationError])

    @annotation.tailrec
    def execute(
        in: Seq[JsResult[_]],
        suc: List[Any],
        fail: collection.Seq[JsFail]
    ): JsResult[List[Any]] = in.headOption match {
      case Some(JsSuccess(v, _)) =>
        execute(in.tail, v :: suc, fail)

      case Some(JsError(details)) =>
        execute(in.tail, suc, details ++: fail)

      case _ =>
        if (fail.nonEmpty) {
          JsError(fail)
        } else {
          JsSuccess(suc.reverse)
        }
    }

    execute(in, List.empty, List.empty)
  }

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
