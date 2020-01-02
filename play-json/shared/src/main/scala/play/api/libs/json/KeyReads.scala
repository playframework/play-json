/*
 * Copyright (C) 2009-2020 Lightbend Inc. <https://www.lightbend.com>
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

object KeyReads extends EnvKeyReads
