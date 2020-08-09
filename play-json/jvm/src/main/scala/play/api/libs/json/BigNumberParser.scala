/*
 * Copyright (C) 2009-2020 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

object BigNumberParser {

  def parseBigDecimal(input: String, settings: JsonParserSettings): java.math.BigDecimal = {

    // There is a limit of how large the numbers can be since parsing extremely
    // large numbers (think thousand of digits) and operating on the parsed values
    // can potentially cause a DDoS.
    if (input.length > settings.bigDecimalParseSettings.digitsLimit) {
      throw new DigitLimitException
    }

    // Must create the BigDecimal with a MathContext that is consistent with the limits used.
    val bigDecimal = new java.math.BigDecimal(input, settings.bigDecimalParseSettings.mathContext)

    // We should also avoid numbers with scale that are out of a safe limit
    if (Math.abs(bigDecimal.scale) > settings.bigDecimalParseSettings.scaleLimit) {
      throw new ScaleLimitException(bigDecimal.scale)
    }

    bigDecimal
  }

  def parseBigInteger(input: String, settings: JsonParserSettings): java.math.BigInteger = {

    if (input.length > settings.bigDecimalParseSettings.digitsLimit) {
      throw new DigitLimitException
    }

    new java.math.BigInteger(input)
  }

  class DigitLimitException extends Exception

  class ScaleLimitException(val scale: Int) extends Exception
}
