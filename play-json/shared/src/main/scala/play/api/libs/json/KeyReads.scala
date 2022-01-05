/*
 * Copyright (C) 2009-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import scala.util.control.NonFatal

/**
 * Used to read object key for types other than `String`.
 *
 * @see [[Reads.keyMapReads]]
 */
trait KeyReads[T] { self =>
  def readKey(key: String): JsResult[T]

  final def map[U](f: T => U): KeyReads[U] = KeyReads[U] { key => self.readKey(key).map(f) }
}

object KeyReads extends EnvKeyReads with LowPriorityKeyReads {

  /**
   * Returns an instance which uses `f` as [[KeyReads.readKey]] function.
   */
  def apply[T](f: String => JsResult[T]): KeyReads[T] = new KeyReads[T] {
    def readKey(key: String) = f(key)
  }

  implicit val charKeyReads: KeyReads[Char] = KeyReads[Char] {
    _.headOption match {
      case Some(ch) => JsSuccess(ch)
      case _        => JsError("error.expected.character")
    }
  }

  implicit val booleanKeyReads: KeyReads[Boolean] = KeyReads[Boolean] {
    case "true"  => JsSuccess(true)
    case "false" => JsSuccess(false)
    case _       => JsError("error.expected.boolean")
  }

  implicit val byteKeyReads: KeyReads[Byte] = charKeyReads.map(_.toByte)

  implicit val shortKeyReads: KeyReads[Short] =
    unsafe[Short]("error.expected.short")(_.toShort)

  implicit val intKeyReads: KeyReads[Int] =
    unsafe[Int]("error.expected.int")(_.toInt)

  implicit val longKeyReads: KeyReads[Long] =
    unsafe[Long]("error.expected.long")(_.toLong)

  implicit val floatKeyReads: KeyReads[Float] =
    unsafe[Float]("error.expected.float")(_.toFloat)

  implicit val doubleKeyReads: KeyReads[Double] =
    unsafe[Double]("error.expected.double")(_.toDouble)

  private def unsafe[T](err: String)(f: String => T): KeyReads[T] =
    KeyReads[T] { key =>
      try {
        JsSuccess(f(key))
      } catch {
        case NonFatal(_) => JsError(err)
      }
    }
}

private[json] sealed trait LowPriorityKeyReads {

  implicit def readableKeyReads[T](implicit
      r: Reads[T]
  ): KeyReads[T] =
    KeyReads[T] { key => r.reads(JsString(key)) }
}
