/*
 * Copyright (C) 2009-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import scala.deriving._

import scala.quoted._

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

  def implicitConfigValueReads[A: Type](using Quotes, Type[Reads]): Expr[Reads[A]] = withSummonedConfig(
    valueImpl[A, Reads](_, "read")
  )

  def implicitConfigWrites[A: Type](using Quotes, Type[OWrites]): Expr[OWrites[A]] = withSummonedConfig(
    macroImpl[A, OWrites, Writes](_, "write", "contramap", reads = false, writes = true)
  )

  def implicitConfigValueWrites[A: Type](using Quotes, Type[Writes]): Expr[Writes[A]] = withSummonedConfig(
    valueImpl[A, Writes](_, "write")
  )

  def implicitConfigFormat[A: Type](using Quotes, Type[Format]): Expr[OFormat[A]] = withSummonedConfig(
    macroImpl[A, OFormat, Format](_, "format", "inmap", reads = true, writes = true)
  )

  def implicitConfigValueFormat[A: Type](using Quotes, Type[Format]): Expr[Format[A]] = withSummonedConfig(
    valueImpl[A, Format](_, "format")
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

  private def valueImpl[A: Type, M[_]: Type](
      config: Expr[JsonConfiguration],
      methodName: String
  )(using q: Quotes): Expr[M[A]] = ???

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
