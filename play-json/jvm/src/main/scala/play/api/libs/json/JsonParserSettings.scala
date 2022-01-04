/*
 * Copyright (C) 2009-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import java.math.MathContext

import scala.util.control.NonFatal

/**
 * Parse settings for BigDecimals. Defines limits that will be used when parsing the BigDecimals, like how many digits
 * are accepted.
 *
 * @param mathContext the [[MathContext]] used when parsing.
 * @param scaleLimit limit the scale, and it is related to the math context used.
 * @param digitsLimit how many digits are accepted, also related to the math context used.
 */
final case class BigDecimalParseSettings(
    mathContext: MathContext = MathContext.DECIMAL128,
    scaleLimit: Int,
    digitsLimit: Int
)

final case class BigDecimalSerializerSettings(
    minPlain: BigDecimal,
    maxPlain: BigDecimal
)

final case class JsonParserSettings(
    bigDecimalParseSettings: BigDecimalParseSettings,
    bigDecimalSerializerSettings: BigDecimalSerializerSettings
)

object JsonParserSettings {
  val defaultMathContext: MathContext = MathContext.DECIMAL128

  // Limit for the scale considering the MathContext of 128
  // limit for scale for decimal128: BigDecimal("0." + "0" * 33 + "1e-6143", java.math.MathContext.DECIMAL128).scale + 1
  val defaultScaleLimit: Int = 6178

  // 307 digits should be the correct value for 128 bytes. But we are using 310
  // because Play JSON uses BigDecimal to parse any number including Doubles and
  // Doubles max value has 309 digits, so we are using 310 here
  val defaultDigitsLimit: Int = 310

  // Maximum magnitude of BigDecimal to write out as a plain string
  val MaxPlain: BigDecimal = 1e20

  // Minimum magnitude of BigDecimal to write out as a plain string
  val MinPlain: BigDecimal = 1e-10

  def apply(): JsonParserSettings = JsonParserSettings(
    BigDecimalParseSettings(defaultMathContext, defaultScaleLimit, defaultDigitsLimit),
    BigDecimalSerializerSettings(minPlain = MinPlain, maxPlain = MaxPlain)
  )

  /**
   * Return the parse settings that are configured.
   */
  val settings: JsonParserSettings = {
    // Initialize the parser settings from System properties. This way it is possible to users
    // to easily replace the default values.
    val scaleLimit  = parseNum("play.json.parser.scaleLimit", defaultScaleLimit)(_.toInt)
    val digitsLimit = parseNum("play.json.parser.digitsLimit", defaultDigitsLimit)(_.toInt)
    val mathContext = parseMathContext("play.json.parser.mathContext")

    val minPlain: BigDecimal = parseNum("play.json.serializer.minPlain", MinPlain)(BigDecimal.exact)
    val maxPlain: BigDecimal = parseNum("play.json.serializer.maxPlain", MaxPlain)(BigDecimal.exact)

    JsonParserSettings(
      BigDecimalParseSettings(
        mathContext,
        scaleLimit,
        digitsLimit
      ),
      BigDecimalSerializerSettings(
        minPlain,
        maxPlain
      )
    )
  }

  private def parseMathContext(key: String): MathContext = sys.props.get(key).map(_.toLowerCase) match {
    case Some("decimal128") => MathContext.DECIMAL128
    case Some("decimal64")  => MathContext.DECIMAL64
    case Some("decimal32")  => MathContext.DECIMAL32
    case Some("unlimited")  => MathContext.UNLIMITED
    case _                  => defaultMathContext
  }

  private def parseNum[T](key: String, default: T)(f: String => T): T =
    try {
      sys.props.get(key).map(f).getOrElse(default)
    } catch {
      case NonFatal(_) => default
    }
}
