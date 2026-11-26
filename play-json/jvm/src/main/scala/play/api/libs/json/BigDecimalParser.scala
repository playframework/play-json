/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import java.math.{ BigDecimal => JBigDecimal }

private[json] object BigDecimalParser {

  def parse(input: String, jsonConfig: JsonConfig): JsResult[JBigDecimal] = {
    import jsonConfig.bigDecimalParseConfig

    // There is a limit of how large the numbers can be since parsing extremely
    // large numbers (think thousand of digits) and operating on the parsed values
    // can potentially cause a DDoS.
    if (input.length > bigDecimalParseConfig.digitsLimit) {
      JsError("error.expected.numberdigitlimit")
    } else {
      // Must create the BigDecimal with a MathContext that is consistent with the limits used.
      try {
        val bigDecimal: JBigDecimal = {
          if (bigDecimalParseConfig.useJacksonParser) {
            com.fasterxml.jackson.core.io.NumberInput.parseBigDecimal(input, bigDecimalParseConfig.useJacksonFastParser)

          } else {
            new JBigDecimal(input, bigDecimalParseConfig.mathContext)
          }
        }

        // We should also avoid numbers with scale that are out of a safe limit
        val scale = bigDecimal.scale

        if (Math.abs(scale) > bigDecimalParseConfig.scaleLimit) {
          JsError(JsonValidationError("error.expected.numberscalelimit", scale))
        } else {
          JsSuccess(bigDecimal)
        }
      } catch {
        case _: NumberFormatException => JsError("error.expected.numberformatexception")
      }
    }
  }
}
