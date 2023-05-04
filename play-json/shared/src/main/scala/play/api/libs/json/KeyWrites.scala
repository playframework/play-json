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

object KeyWrites extends EnvKeyWrites {

  /**
   * Returns an instance which uses `f` as [[KeyWrites.writeKey]] function.
   */
  def apply[T](f: T => String): KeyWrites[T] = new KeyWrites[T] {
    def writeKey(key: T) = f(key)
  }

  implicit def anyValKeyWrites[T <: AnyVal]: KeyWrites[T] =
    KeyWrites[T](_.toString)
}
