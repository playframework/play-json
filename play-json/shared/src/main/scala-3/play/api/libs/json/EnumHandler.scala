/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import scala.util.{ Failure, Success }

import scala.deriving.Mirror
import scala.reflect.Enum

/**
 * Utilities to handle [[https://dotty.epfl.ch/docs/reference/enums/enums.html Enumerations]]
 *
 * (Inspired by [[https://github.com/lloydmeta/enumeratum/blob/master/enumeratum-reactivemongo-bson/src/main/scala/enumeratum/EnumHandler.scala enumeratum]])
 */
private[json] trait EnumHandler {

  /**
   * Creates a `Reads[A]`, if `A` is a `Enum`,
   * by resolving at compile-time the `Reads` for the underlying type.
   *
   * $macroWarning
   *
   * $macroTypeParam
   *
   * {{{
   * import play.api.libs.json.{ Json, Reads }
   *
   * enum Color:
   *   case Red, Green, Blue
   *
   * val r: Reads[Color] = Json.enumReads
   * // r.reads(JsString("Red")) => JsSuccess(Color.Red)
   * }}}
   */
  inline def enumReads[E <: Enum: Mirror.SumOf]: Reads[E] =
    collect[E](EnumHelper.strictValueOf[E])

  /**
   * Creates a `Reads[A]`, if `A` is a `Enum`,
   * by resolving at compile-time the `Reads` for the underlying type.
   *
   * @param insensitive bind in a case-insensitive way, defaults to false
   *
   * $macroWarning
   *
   * $macroTypeParam
   *
   * {{{
   * import play.api.libs.json.{ Json, Reads }
   *
   * enum Color:
   *   case Red, Green, Blue
   *
   * val r: Reads[Color] = Json.enumReads
   * }}}
   */
  inline def enumReads[E <: Enum: Mirror.SumOf](
      insensitive: Boolean = false
  ): Reads[E] = {
    if (insensitive) {
      collect[E](EnumHelper.insensitiveValueOf[E])
    } else {
      collect[E](EnumHelper.strictValueOf[E])
    }
  }

  /**
   * Returns a strict `KeyReads` for a given enum.
   */
  inline def keyEnumReads[E <: Enum: Mirror.SumOf]: KeyReads[E] =
    collectKey[E](EnumHelper.strictValueOf[E])

  /**
   * Returns a `KeyReads` for a given enum.
   *
   * @param insensitive bind in a case-insensitive way, defaults to false
   */
  inline def keyEnumReads[E <: Enum: Mirror.SumOf](
      insensitive: Boolean = false
  ): KeyReads[E] = {
    if (insensitive) {
      collectKey[E](EnumHelper.insensitiveValueOf[E])
    } else {
      collectKey[E](EnumHelper.strictValueOf[E])
    }
  }

  /**
   * Creates a `Reads[A]` for a given enum transformed to lower case,
   * by resolving at compile-time the `Reads` for the underlying type.
   *
   * $macroWarning
   *
   * $macroTypeParam
   *
   * {{{
   * import play.api.libs.json.{ Json, Reads }
   *
   * enum Color:
   *   case Red, Green, Blue
   *
   * val r: Reads[Color] = Json.enumReads
   * }}}
   */
  inline def enumReadsLowercaseOnly[E <: Enum: Mirror.SumOf]: Reads[E] =
    collect[E](EnumHelper.lowerCaseValueOf[E])

  /**
   * Returns a `KeyReads` for a given enum transformed to lower case.
   */
  inline def keyEnumReadsLowercaseOnly[E <: Enum: Mirror.SumOf]: KeyReads[E] =
    collectKey[E](EnumHelper.lowerCaseValueOf[E])

  /**
   * Creates a `Reads[A]` for a given enum transformed to upper case,
   * by resolving at compile-time the `Reads` for the underlying type.
   *
   * $macroWarning
   *
   * $macroTypeParam
   *
   * {{{
   * import play.api.libs.json.{ Json, Reads }
   *
   * enum Color:
   *   case Red, Green, Blue
   *
   * val r: Reads[Color] = Json.enumReads
   * }}}
   */
  inline def enumReadsUppercaseOnly[E <: Enum: Mirror.SumOf]: Reads[E] =
    collect[E](EnumHelper.upperCaseValueOf[E])

  /**
   * Returns a `KeyReads` for a given enum transformed to upper case.
   */
  inline def keyEnumReadsUppercaseOnly[E <: Enum: Mirror.SumOf]: KeyReads[E] =
    collectKey[E](EnumHelper.upperCaseValueOf[E])

  private def collect[E <: Enum](f: String => Option[E]): Reads[E] =
    Reads[E] {
      case JsString(str) =>
        f(str) match {
          case Some(v) => JsSuccess(v)

          case None => JsError("error.expected.enum")
        }

      case _ =>
        JsError("error.expected.string")
    }

  private def collectKey[E <: Enum](f: String => Option[E]): KeyReads[E] =
    KeyReads[E] { key =>
      f(key) match {
        case Some(v) => JsSuccess(v)

        case None => JsError("error.expected.enum")
      }
    }

  /**
   * Creates a `Writes[T]`, if `A` is a `Enum`, by writing its string representation.
   *
   * $macroWarning
   *
   * $macroTypeParam
   *
   * {{{
   * import play.api.libs.json.{ Json, Writes }
   *
   * enum Color:
   *   case Red, Green, Blue
   *
   * val w: Writes[Color] = Json.enumWrites
   *
   * w.writes(Color.Green) // "Green"
   * }}}
   */
  def enumWrites[E <: Enum]: Writes[E] =
    Writes[E] { entry => JsString(entry.productPrefix) }

  /**
   * Returns a `KeyWrites` for a given enum.
   */
  def keyEnumWrites[E <: Enum]: KeyWrites[E] = KeyWrites[E](_.productPrefix)

  /**
   * Creates a `Writes[T]`, if `A` is a `Enum`, by writing it as lower case.
   *
   * $macroWarning
   *
   * $macroTypeParam
   *
   * {{{
   * import play.api.libs.json.{ Json, Writes }
   *
   * enum Color:
   *   case Red, Green, Blue
   *
   * val w: Writes[Color] = Json.enumWrites
   *
   * w.writes(Color.Green) // "Green"
   * }}}
   */
  def enumWritesLowercase[E <: Enum]: Writes[E] =
    Writes[E] { entry => JsString(entry.productPrefix.toLowerCase) }

  /**
   * Returns a `KeyWrites` for a given enum, the value as lower case.
   */
  def keyEnumWritesLowercase[E <: Enum]: KeyWrites[E] =
    KeyWrites[E](_.productPrefix.toLowerCase)

  /**
   * Creates a `Writes[T]`, if `A` is a `Enum`, by writing it as upper case.
   *
   * $macroWarning
   *
   * $macroTypeParam
   *
   * {{{
   * import play.api.libs.json.{ Json, Writes }
   *
   * enum Color:
   *   case Red, Green, Blue
   *
   * val w: Writes[Color] = Json.enumWrites
   *
   * w.writes(Color.Green) // "Green"
   * }}}
   */
  def enumWritesUppercase[E <: Enum]: Writes[E] =
    Writes[E] { entry => JsString(entry.productPrefix.toUpperCase) }

  /**
   * Returns a `KeyWrites` for a given enum, the value as upper case.
   */
  def keyEnumWritesUppercase[E <: Enum]: KeyWrites[E] =
    KeyWrites[E](_.productPrefix.toUpperCase)

  /**
   * Creates a `Format[A]`, if `A` is a `Enum`.
   *
   * $macroWarning
   *
   * $macroTypeParam
   *
   * {{{
   * import play.api.libs.json.{ Json, Format }
   *
   * enum Color:
   *   case Red, Green, Blue
   *
   * val fmt: Format[Color] = Json.format
   * }}}
   */
  inline def enumFormat[E <: Enum: Mirror.SumOf]: Format[E] =
    enumFormat[E](false)

  /**
   * Returns a `Format` for a given enum.
   *
   * @param insensitive bind in a case-insensitive way, defaults to false
   */
  inline def enumFormat[E <: Enum: Mirror.SumOf](
      insensitive: Boolean = false
  ): Format[E] =
    Format[E](enumReads[E](insensitive), enumWrites[E])

  /**
   * Returns a `Format` for a given enum,
   * handling a lower case transformation.
   */
  inline def enumFormatLowercaseOnly[E <: Enum: Mirror.SumOf]: Format[E] =
    Format[E](enumReadsLowercaseOnly[E], enumWritesLowercase[E])

  /**
   * Returns a `Format` for a given enum,
   * handling an upper case transformation.
   */
  inline def enumFormatUppercaseOnly[E <: Enum: Mirror.SumOf]: Format[E] =
    Format[E](enumReadsUppercaseOnly[E], enumWritesUppercase[E])

}
