/*
 * Copyright (C) 2009-2020 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import scala.util.control

trait EnvReads {

  protected def parseBigDecimal(input: String): JsResult[java.math.BigDecimal] = {
    control.Exception
      .catching(classOf[NumberFormatException])
      .opt(JsSuccess(new java.math.BigDecimal(input)))
      .getOrElse(JsError(JsonValidationError("error.expected.numberformatexception")))
  }

  protected def parseBigInteger(input: String): JsResult[java.math.BigInteger] = {
    control.Exception
      .catching(classOf[NumberFormatException])
      .opt(JsSuccess(new java.math.BigInteger(input)))
      .getOrElse(JsError(JsonValidationError("error.expected.numberformatexception")))
  }
}

trait EnvKeyReads { _: KeyReads.type =>
  // No specific reader
}
