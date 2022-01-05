/*
 * Copyright (C) 2009-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

/**
 * A JSON validation error representation.
 */
case class JsonValidationError(messages: Seq[String], args: Any*) {
  lazy val message = messages.last
}

object JsonValidationError {

  /**
   * {{{
   * import play.api.libs.json.JsonValidationError
   *
   * val simpleError = JsonValidationError("error.key1")
   *
   * val detailedError = JsonValidationError("error.key1", "details")
   * }}}
   */
  def apply(message: String, args: Any*): JsonValidationError =
    JsonValidationError(Seq(message), args: _*)

  private[json] val PathMissing = Seq(JsonValidationError("error.path.missing"))

  /**
   * Extracts the first error message.
   *
   * {{{
   * import play.api.libs.json.JsonValidationError
   *
   * def msg(err: JsonValidationError): Option[String] = err match {
   *   case JsonValidationError.Message(msg) => Some(msg)
   *   case _ => None
   * }
   * }}}
   */
  object Message {

    def unapply(error: JsonValidationError): Option[String] =
      error.messages.headOption
  }

  /**
   * Extracts the first error details (message and its first argument).
   *
   * {{{
   * import play.api.libs.json.JsonValidationError
   *
   * def details(err: JsonValidationError): Option[(String, Any)] = err match {
   *   case JsonValidationError.Detailed(msg, arg) => Some(msg -> arg)
   *   case _ => None
   * }
   * }}}
   */
  object Detailed {

    def unapply(error: JsonValidationError): Option[(String, Any)] =
      for {
        msg <- error.messages.headOption
        arg <- error.args.headOption
      } yield msg -> arg
  }
}
