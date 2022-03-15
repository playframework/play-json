/*
 * Copyright (C) 2009-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import scala.language.experimental.macros

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
  def reads[A]: Reads[A] = macro JsMacroImpl.implicitConfigReadsImpl[A]

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
  def writes[A]: OWrites[A] = macro JsMacroImpl.implicitConfigWritesImpl[A]

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
  def format[A]: OFormat[A] = macro JsMacroImpl.implicitConfigFormatImpl[A]

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
  def valueReads[A]: Reads[A] = macro JsMacroImpl.implicitConfigValueReads[A]

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
  def valueWrites[A]: Writes[A] = macro JsMacroImpl.implicitConfigValueWrites[A]

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
  def valueFormat[A]: Format[A] = macro JsMacroImpl.implicitConfigValueFormat[A]

}

trait JsMacrosWithOptions[Opts <: Json.MacroOptions] extends JsMacros {
  override def reads[A]: Reads[A] = macro JsMacroImpl.withOptionsReadsImpl[A]
  override def writes[A]: OWrites[A] = macro JsMacroImpl.withOptionsWritesImpl[A]
  override def format[A]: OFormat[A] = macro JsMacroImpl.withOptionsFormatImpl[A]
}
