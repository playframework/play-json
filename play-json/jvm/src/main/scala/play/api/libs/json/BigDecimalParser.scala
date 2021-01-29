/*
 * Copyright (C) 2009-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

private[json] object BigDecimalParser {

  def parse(input: String, settings: JsonParserSettings): JsResult[java.math.BigDecimal] = {

    // There is a limit of how large the numbers can be since parsing extremely
    // large numbers (think thousand of digits) and operating on the parsed values
    // can potentially cause a DDoS.
    if (input.length > settings.bigDecimalParseSettings.digitsLimit) {
      JsError("error.expected.numberdigitlimit")
    } else {
      // Must create the BigDecimal with a MathContext that is consistent with the limits used.
      try {
        val bigDecimal = new java.math.BigDecimal(input, settings.bigDecimalParseSettings.mathContext)

        // We should also avoid numbers with scale that are out of a safe limit
        val scale = bigDecimal.scale
        if (Math.abs(scale) > settings.bigDecimalParseSettings.scaleLimit) {
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
