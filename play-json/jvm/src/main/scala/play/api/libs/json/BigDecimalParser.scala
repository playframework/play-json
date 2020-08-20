/*
 * Copyright (C) 2009-2020 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import scala.util.control.NonFatal

object BigDecimalParser {

  def parse(input: String, settings: JsonParserSettings): Either[Error, java.math.BigDecimal] = {

    // There is a limit of how large the numbers can be since parsing extremely
    // large numbers (think thousand of digits) and operating on the parsed values
    // can potentially cause a DDoS.
    if (input.length > settings.bigDecimalParseSettings.digitsLimit) {
      Left(Error.DigitLimitError())
    } else {
      // Must create the BigDecimal with a MathContext that is consistent with the limits used.
      try {
        val bigDecimal = new java.math.BigDecimal(input, settings.bigDecimalParseSettings.mathContext)

        // We should also avoid numbers with scale that are out of a safe limit
        if (Math.abs(bigDecimal.scale) > settings.bigDecimalParseSettings.scaleLimit) {
          Left(Error.ScaleLimitError(bigDecimal.scale))
        } else {
          Right(bigDecimal)
        }

      } catch {
        case NonFatal(e) => Left(Error.ParsingError(e))
      }
    }
  }

  sealed trait Error

  object Error {

    case class DigitLimitError() extends Error

    case class ScaleLimitError(scale: Int) extends Error

    case class ParsingError(cause: Throwable) extends Error
  }
}
