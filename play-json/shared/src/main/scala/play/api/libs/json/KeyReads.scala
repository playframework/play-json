/*
 * Copyright (C) 2009-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

/**
 * Used to read object key for types other than `String`.
 *
 * @see [[Reads.keyMapReads]]
 */
trait KeyReads[T] {
  def readKey(key: String): JsResult[T]
}

object KeyReads extends EnvKeyReads {

  /**
   * Returns an instance which uses `f` as [[KeyReads.readKey]] function.
   */
  def apply[T](f: String => JsResult[T]): KeyReads[T] = new KeyReads[T] {
    def readKey(key: String) = f(key)
  }
}
