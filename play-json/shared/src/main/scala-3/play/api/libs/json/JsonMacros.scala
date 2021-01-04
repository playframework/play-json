/*
 * Copyright (C) 2009-2020 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import scala.language.experimental.macros
import scala.deriving.{ArrayProduct, Mirror}
import scala.compiletime.{erasedValue, summonInline, summonFrom, constValue, error,codeOf}


import _root_.play.api.libs.functional.syntax._

trait JsonMacros {
  
  inline def summonLabels[T <: Tuple]: List[String] = {
    inline erasedValue[T] match {
      case _: EmptyTuple => Nil
      case _: (t *: ts) => constValue[t].asInstanceOf[String] :: summonLabels[ts]
    }
  }

  inline final def summonLabelAndReads[L, R]: Reads[R] = {
    def path = (__ \ implicitly[JsonConfiguration].naming(constValue[L].asInstanceOf[String]))
    inline erasedValue[R] match {
      case _: Option[a] => path.readNullable(summonReads[a]).asInstanceOf[Reads[R]]
      case _: R => path.read(summonReads[R])
    }
  }

  inline final def summonReads[A]: Reads[A] = {
    inline erasedValue[A] match {
      //case _: Option[a] => Reads.optionNoError[a](summonReads[a]).asInstanceOf[Reads[A]]
      //case _: Either[l,r] => Reads.apply(_ => JsError("fixme either"))
      case _: A => summonInline[Reads[A]]
    } 
  }

  inline def summonAllReads[T <: Tuple]: List[Reads[_]] = {
    inline erasedValue[T] match {
      case _: EmptyTuple => Nil
      case _: ((l,t) *: ts) => summonLabelAndReads[l,t] :: summonAllReads[ts]
      case _: Tuple => Nil
    }
  }

  inline final def summonLabelAndWrites[L, R]: OWrites[R] = {
    def path = (__ \ implicitly[JsonConfiguration].naming(constValue[L].asInstanceOf[String]))
    inline erasedValue[R] match {
      case _: Option[a] => path.writeNullable(summonWrites[a]).asInstanceOf[OWrites[R]]
      case _: R => path.write(summonWrites[R])
    }
  }

  inline final def summonWrites[A]: Writes[A] = {
    // inline erasedValue[A] match {
    //   //case _: Either[l,r] => Writes.apply(_ => ???)
    //   case _: A => summonInline[Writes[A]]
    // } 
    summonInline[Writes[A]]
  }

  inline def summonAllWrites[T <: Tuple]: List[OWrites[_]] = {
    inline erasedValue[T] match {
      case _: EmptyTuple => Nil
      case _: ((l,t) *: ts) => summonLabelAndWrites[l,t] :: summonAllWrites[ts]
      case _: Tuple => Nil
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
    val x = summonAllReads[Tuple.Zip[m.MirroredElemLabels, m.MirroredElemTypes]]
    inline m match {
      case s: Mirror.SumOf[A]     => readsSum(s)
      case p: Mirror.ProductOf[A] => readsProduct(p, x)
    }
  }
  
  private def readsSum[A](s: Mirror.SumOf[A]): Reads[A] = {
    Reads.apply(_ => JsError())
  }
  private def readsProduct[A](p: Mirror.ProductOf[A], reads: List[Reads[_]]): Reads[A] = {    
    val xpto: Reads[Tuple] = reads.foldRight(Reads.JsObjectReads.map(_ => EmptyTuple)){ case (e, r) =>
      e.flatMap(e => r.map(e *: _))
    }

    xpto.map{t => p.fromProduct(t)}
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
  inline def valueReads[A <: AnyVal]: Reads[A] = Reads.apply(_ => JsError()) //macro JsMacroImpl.implicitConfigValueReads[A]

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
  inline def writes[A](using m: Mirror.Of[A]): OWrites[A] = {
    val x = summonAllWrites[Tuple.Zip[m.MirroredElemLabels, m.MirroredElemTypes]]
    inline m match {
      case s: Mirror.SumOf[A]     => writesSum(s)
      case p: Mirror.ProductOf[A] => writesProduct(p, x)
    }
  }

  private def writesSum[A](s: Mirror.SumOf[A]): OWrites[A] = {
    OWrites.apply(_ => ???)
  }
  private def writesProduct[A](p: Mirror.ProductOf[A], writes: List[OWrites[_]]): OWrites[A] = {    
    OWrites[A].apply{ a =>
      a.asInstanceOf[Product].productIterator.zip(writes).foldRight(JsObject.empty){case ((a,w),b) => b ++ w.asInstanceOf[OWrites[Any]].writes(a) }
    }

  }

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
  inline def valueWrites[A <: AnyVal]: Writes[A] = Writes.apply(a => JsNull) //macro JsMacroImpl.implicitConfigValueWrites[A]

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
  inline def valueFormat[A <: AnyVal]: Format[A] = Format.apply(valueReads, valueWrites) //macro JsMacroImpl.implicitConfigValueFormat[A]

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
