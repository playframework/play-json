/*
 * Copyright (C) 2009-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import scala.deriving._

private[json] trait JsMacros {

  /**
   * Creates a `Reads[T]` by resolving, at compile-time,
   * the case class fields or sealed family, and the required implicits.
   *
   * $macroWarning
   *
   * $macroTypeParam
   *
   * {{{
   * import play.api.libs.json.{ Json, Reads }
   *
   * case class User(userName: String, age: Int)
   *
   * implicit val userReads: Reads[User] =
   *   Json.using[Json.MacroOptions with Json.DefaultValues].reads[User]
   * }}}
   */
  inline def reads[A]: Reads[A] = ${ JsMacroImpl.reads[A] }

  /**
   * Creates a `OWrites[T]` by resolving, at compile-time,
   * the case class fields or sealed family, and the required implicits.
   *
   * $macroWarning
   *
   * $macroTypeParam
   *
   * {{{
   * import play.api.libs.functional.syntax._
   * import play.api.libs.json.{ Json, JsonConfiguration, __ }
   *
   * case class User(userName: String, age: Int)
   *
   * implicit val userWrites1 = Json.writes[User]
   * // macro-compiler replaces Json.writes[User] by injecting into compile chain
   * // the exact code you would write yourself. This is strictly equivalent to:
   * implicit val userWrites2 = (
   *    (__ \ implicitly[JsonConfiguration].naming("userName")).write[String] and
   *    (__ \ implicitly[JsonConfiguration].naming("age")).write[Int]
   * )(unlift(User.unapply))
   * }}}
   */
  inline def writes[A]: OWrites[A] = ${ JsMacroImpl.writes[A] }

  /**
   * Creates a `OFormat[T]` by resolving, at compile-time,
   * the case class fields or sealed family, and the required implicits.
   *
   * $macroWarning
   *
   * $macroTypeParam
   *
   * {{{
   * import play.api.libs.functional.syntax._
   * import play.api.libs.json.{ Json, JsonConfiguration, __ }
   *
   * case class User(userName: String, age: Int)
   *
   * val userFormat1 = Json.format[User]
   * // macro-compiler replaces Json.format[User] by injecting into compile chain
   * // the exact code you would write yourself. This is strictly equivalent to:
   * val userFormat2 = (
   *    (__ \ implicitly[JsonConfiguration].naming("userName")).format[String] and
   *    (__ \ implicitly[JsonConfiguration].naming("age")).format[Int]
   * )(User.apply, unlift(User.unapply))
   * }}}
   */
  inline def format[A]: OFormat[A] = ${ JsMacroImpl.format[A] }
}

private[json] trait JsValueMacros {

  /**
   * Creates a `Reads[A]`, if `A` is a ValueClass,
   * by resolving at compile-time the `Reads` for the underlying type.
   *
   * $macroWarning
   *
   * $macroTypeParam
   *
   * {{{
   * import play.api.libs.json.{ Json, Reads }
   *
   * final class IdText(val value: String) extends AnyVal
   *
   * // Based on provided Reads[String] corresponding to `value: String`
   * val r: Reads[IdText] = Json.valueReads
   * }}}
   */
  inline def valueReads[A <: AnyVal]: Reads[A] =
    ${ JsMacroImpl.anyValReads[A] }

  /**
   * Creates a `OWrites[T]`, if `T` is a ValueClass,
   * by resolving at compile-time the `Writes` for the underlying type.
   *
   * $macroWarning
   *
   * $macroTypeParam
   *
   * {{{
   * import play.api.libs.json.{ Json, Writes }
   *
   * final class TextId(val value: String) extends AnyVal
   *
   * // Based on provided Writes[String] corresponding to `value: String`
   * val w: Writes[TextId] = Json.valueWrites[TextId]
   * }}}
   */
  inline def valueWrites[A <: AnyVal]: Writes[A] =
    ${ JsMacroImpl.anyValWrites[A] }

  /**
   * Creates a `OFormat[T]` by resolving, if `T` is a ValueClass
   * (see [[valueReads]] and [[valueWrites]]).
   *
   * $macroWarning
   *
   * $macroTypeParam
   *
   * {{{
   * import play.api.libs.json.{ Format, Json }
   *
   * final class User(val name: String) extends AnyVal
   *
   * implicit val userFormat: Format[User] = Json.valueFormat[User]
   * }}}
   */
  inline def valueFormat[A <: AnyVal]: Format[A] =
    ${ JsMacroImpl.anyValFormat[A] }

  // ---

  /** Only for internal purposes */
  final class Placeholder {} // TODO: Common with Scala-2

  /** Only for internal purposes */
  object Placeholder {

    implicit object Format extends OFormat[Placeholder] {

      val success =
        JsSuccess(new Placeholder())

      def reads(json: JsValue): JsResult[Placeholder] = success

      def writes(pl: Placeholder) = Json.obj()
    }
  }
}

trait JsMacrosWithOptions[Opts <: Json.MacroOptions] {
  withOpts: Json.WithOptions[Opts] =>

  inline def reads[A]: Reads[A] =
    ${ JsMacroImpl.withOptionsReads[A, Opts]('config) }

  inline def writes[A]: OWrites[A] =
    ${ JsMacroImpl.withOptionsWrites[A, Opts]('config) }

  inline def format[A]: OFormat[A] =
    ${ JsMacroImpl.withOptionsFormat[A, Opts]('config) }
}
