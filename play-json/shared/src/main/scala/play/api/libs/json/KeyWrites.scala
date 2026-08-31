/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

/**
 * Used to write object key for types other than `String`.
 *
 * @see [[Writes.keyMapWrites]]
 */
trait KeyWrites[T] {
  def writeKey(key: T): String
}

object KeyWrites extends EnvKeyWrites with LowPriorityKeyWrites {

  /**
   * Returns an instance which uses `f` as [[KeyWrites.writeKey]] function.
   */
  def apply[T](f: T => String): KeyWrites[T] = new KeyWrites[T] {
    def writeKey(key: T) = f(key)
  }

  /**
   * Creates a [[KeyWrites]] for values represented as JSON strings.
   *
   * The supplied [[Writes]] is expected to produce a [[JsString]] for every value.
   * This method is unsafe in that an [[IllegalArgumentException]] is thrown if the
   * [[Writes]] produces any other JSON value.
   *
   * @tparam T the type to write
   * @return a [[KeyWrites]] for `T`
   */
  implicit def stringRepresentedKeyWrites[T](implicit
      w: Writes[T],
      repr: Format.Representation[T, JsString]
  ): KeyWrites[T] = KeyWrites[T] {
    w.writes(_) match {
      case JsString(str) =>
        str

      case js =>
        throw new IllegalArgumentException(s"${Json.prettyPrint(js)} is not represented as JSON string")
    }
  }
}

private[json] sealed trait LowPriorityKeyWrites {
  implicit def anyValKeyWrites[T <: AnyVal]: KeyWrites[T] =
    KeyWrites[T](_.toString)

}
