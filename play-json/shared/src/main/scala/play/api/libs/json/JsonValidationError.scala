/*
 * Copyright (C) 2009-2020 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

/**
 * A JSON validation error representation.
 */
case class JsonValidationError(messages: Seq[String], args: Any*) {
  lazy val message = messages.last
}

object JsonValidationError {
  def apply(message: String, args: Any*): JsonValidationError =
    JsonValidationError(Seq(message), args: _*)

  private[json] val PathMissing = Seq(JsonValidationError("error.path.missing"))
}
