package play.api.libs.json

import scala.language.experimental.macros
import scala.deriving.{ArrayProduct, Mirror}
import scala.compiletime.{erasedValue, summonInline, summonFrom, constValue}

import _root_.play.api.libs.functional.syntax._

trait JsonMacros {

inline final def summonDecoder[A]: Reads[A] = {
  inline erasedValue[A] match {
    case _: Option[a] => Reads.optionNoError[a](summonDecoder[a]).asInstanceOf[Reads[A]]
    case _: Either[l,r] => Reads.apply(_ => JsError())
    case _: A => summonInline[Reads[A]]
  } 
}

  inline def summonAll[T <: Tuple]: List[Reads[_]] = {
    inline erasedValue[T] match {
      case _: EmptyTuple => Nil
      case _: (t *: ts) => summonDecoder[t] :: summonAll[ts]
    }
  }

  inline def summonLabels[T <: Tuple]: List[String] = {
    inline erasedValue[T] match {
      case _: EmptyTuple => Nil
      case _: (t *: ts) => constValue[t].asInstanceOf[String] :: summonLabels[ts]
    }
  }


  /**
   * Creates a `Reads[A]` by resolving, at compile-time,
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
   * implicit val userReads1 = Json.reads[User]
   * // macro-compiler replaces Json.reads[User] by injecting into compile chain
   * // the exact code you would write yourself. This is strictly equivalent to:
   * implicit val userReads2 = (
   *    (__ \ implicitly[JsonConfiguration].naming("userName")).read[String] and
   *    (__ \ implicitly[JsonConfiguration].naming("age")).read[Int]
   * )(User)
   * }}}
   */
  inline def reads[A](using m: Mirror.Of[A]): Reads[A] = {
    val x = summonAll[m.MirroredElemTypes]
    val labels = summonLabels[m.MirroredElemLabels]
    inline m match {
      case s: Mirror.SumOf[A]     => readsSum(s)
      case p: Mirror.ProductOf[A] => readsProduct(p, x, labels)
    }
  }
  
  private def readsSum[A](s: Mirror.SumOf[A]): Reads[A] = {
    Reads.apply(_ => JsError())
  }
  private def readsProduct[A](p: Mirror.ProductOf[A], x: List[Reads[_]], labels: List[String]): Reads[A] = {
    val z = labels.zip(x).map{ case (label, reads) => (__ \ implicitly[JsonConfiguration].naming(label)).read(reads) } //TODO lazyread?
    
    val xpto: Reads[Tuple] = z.foldRight(Reads.pure(EmptyTuple)){ case (e, r) =>
      e.flatMap(e => r.map(e *: _))
    }

    xpto.map(p.fromProduct)
  }

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
  inline def valueReads[A]: Reads[A] = Reads.apply(_ => JsError()) //macro JsMacroImpl.implicitConfigValueReads[A]

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
  inline def writes[A](using m: Mirror.Of[A]): OWrites[A] = OWrites.apply(_ => JsObject.empty) //macro JsMacroImpl.implicitConfigWritesImpl[A]

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
  inline def valueWrites[A]: Writes[A] = Writes.apply(_ => JsNull) //macro JsMacroImpl.implicitConfigValueWrites[A]

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
  inline def format[A](using m: Mirror.Of[A]): OFormat[A] = OFormat.apply(reads, writes) //macro JsMacroImpl.implicitConfigFormatImpl[A]

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
  inline def valueFormat[A]: Format[A] = Format.apply(valueReads, valueWrites) //macro JsMacroImpl.implicitConfigValueFormat[A]

}

trait JsonMacrosWithOptions[Opts <: Json.MacroOptions] { self: Json.WithOptions[Opts] =>

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
  inline def reads[A](using m: Mirror.Of[A]): Reads[A] = {
    implicit val conf: JsonConfiguration = self.config
    Json.reads(using m) //macro JsMacroImpl.withOptionsReadsImpl[A]
  }

  /**
   * Creates a `OWrites[T]` by resolving, at compile-time,
   * the case class fields or sealed family, and the required implicits.
   *
   * $macroWarning
   *
   * $macroTypeParam
   *
   * {{{
   * import play.api.libs.json.{ Json, OWrites }
   *
   * case class User(userName: String, age: Int)
   *
   * implicit val userWrites: OWrites[User] =
   *   Json.using[Json.MacroOptions].writes[User]
   * }}}
   */
  inline def writes[A](using m: Mirror.Of[A]): OWrites[A] = OWrites.apply(_ => JsObject.empty) // macro JsMacroImpl.withOptionsWritesImpl[A]

  /**
   * Creates a `OFormat[T]` by resolving, at compile-time,
   * the case class fields or sealed family, and the required implicits.
   *
   * $macroWarning
   *
   * $macroTypeParam
   *
   * {{{
   * import play.api.libs.json.{ Json, OFormat }
   *
   * case class User(userName: String, age: Int)
   *
   * implicit val userFormat: OFormat[User] =
   *   Json.using[Json.WithDefaultValues].format[User]
   * }}}
   */
  inline def format[A](using m: Mirror.Of[A]): OFormat[A] = OFormat(reads, writes) // macro JsMacroImpl.withOptionsFormatImpl[A]
}
