/*
 * Copyright (C) 2009-2020 Lightbend Inc. <https://www.lightbend.com>
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

object KeyWrites extends EnvKeyWrites
