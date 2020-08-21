/*
 * Copyright (C) 2009-2020 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

trait EnvReads {

  protected def parseBigDecimal(input: String): JsResult[java.math.BigDecimal] = {
    try JsSuccess(new java.math.BigDecimal(input))
    catch {
      case _: NumberFormatException =>
        JsError(JsonValidationError("error.expected.numberformatexception"))
    }
  }

  protected def parseBigInteger(input: String): JsResult[java.math.BigInteger] = {
    try JsSuccess(new java.math.BigInteger(input))
    catch {
      case _: NumberFormatException =>
        JsError(JsonValidationError("error.expected.numberformatexception"))
    }
  }
}

trait EnvKeyReads { _: KeyReads.type =>
  // No specific reader
}
