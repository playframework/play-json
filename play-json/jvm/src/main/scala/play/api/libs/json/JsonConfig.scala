/*
 * Copyright (C) 2009-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import play.api.libs.json.JsonConfig.defaultMaxPlain
import play.api.libs.json.JsonConfig.defaultMinPlain
import play.api.libs.json.JsonConfig.defaultDigitsLimit
import play.api.libs.json.JsonConfig.defaultMathContext
import play.api.libs.json.JsonConfig.defaultPreserveZeroDecimal
import play.api.libs.json.JsonConfig.defaultScaleLimit
import play.api.libs.json.JsonConfig.loadDigitsLimit
import play.api.libs.json.JsonConfig.loadMathContext
import play.api.libs.json.JsonConfig.loadMaxPlain
import play.api.libs.json.JsonConfig.loadMinPlain
import play.api.libs.json.JsonConfig.loadScaleLimit

import java.math.MathContext

import scala.util.control.NonFatal

/**
 * Parse settings for BigDecimals. Defines limits that will be used when parsing the BigDecimals, like how many digits
 * are accepted.
 */
sealed trait BigDecimalParseConfig {

  /** The [[MathContext]] used when parsing. */
  def mathContext: MathContext

  /** Limits the scale, and it is related to the math context used. */
  def scaleLimit: Int

  /** How many digits are accepted, also related to the math context used. */
  def digitsLimit: Int
}

object BigDecimalParseConfig {
  def apply(
      mathContext: MathContext = defaultMathContext,
      scaleLimit: Int = defaultScaleLimit,
      digitsLimit: Int = defaultDigitsLimit
  ): BigDecimalParseConfig = BigDecimalParseConfigImpl(mathContext, scaleLimit, digitsLimit)
}

private final case class BigDecimalParseConfigImpl(mathContext: MathContext, scaleLimit: Int, digitsLimit: Int)
    extends BigDecimalParseConfig

sealed trait BigDecimalSerializerConfig {

  /** Minimum magnitude of BigDecimal to write out as a plain string. */
  def minPlain: BigDecimal

  /** Maximum magnitude of BigDecimal to write out as a plain string. */
  def maxPlain: BigDecimal

  /** True to preserve zero decimals (false by default). */
  def preserveZeroDecimal: Boolean
}

object BigDecimalSerializerConfig {
  def apply(
      minPlain: BigDecimal = defaultMinPlain,
      maxPlain: BigDecimal = defaultMaxPlain,
      preserveZeroDecimal: Boolean = defaultPreserveZeroDecimal
  ): BigDecimalSerializerConfig =
    DecimalSerializerSettingsImpl(minPlain, maxPlain, preserveZeroDecimal)
}

private final case class DecimalSerializerSettingsImpl(
    minPlain: BigDecimal,
    maxPlain: BigDecimal,
    preserveZeroDecimal: Boolean
) extends BigDecimalSerializerConfig

sealed trait JsonConfig {
  def bigDecimalParseConfig: BigDecimalParseConfig
  def bigDecimalSerializerConfig: BigDecimalSerializerConfig
}

object JsonConfig {
  val defaultMathContext: MathContext = MathContext.DECIMAL128

  // Limit for the scale considering the MathContext of 128
  // limit for scale for decimal128: BigDecimal("0." + "0" * 33 + "1e-6143", java.math.MathContext.DECIMAL128).scale + 1
  val defaultScaleLimit: Int = 6178

  // 307 digits should be the correct value for 128 bytes. But we are using 310
  // because Play JSON uses BigDecimal to parse any number including Doubles and
  // Doubles max value has 309 digits, so we are using 310 here
  val defaultDigitsLimit: Int = 310

  // Drop zero decimal by default.
  val defaultPreserveZeroDecimal: Boolean = false

  // Maximum magnitude of BigDecimal to write out as a plain string
  val defaultMaxPlain: BigDecimal = 1E20

  // Minimum magnitude of BigDecimal to write out as a plain string
  val defaultMinPlain: BigDecimal = 1E-10

  private[json] def loadScaleLimit: Int  = parseNum("play.json.parser.scaleLimit", defaultScaleLimit)(_.toInt)

  private[json] def loadDigitsLimit: Int = parseNum("play.json.parser.digitsLimit", defaultDigitsLimit)(_.toInt)

  private[json] def loadMathContext: MathContext = parseMathContext("play.json.parser.mathContext")

  private[json] def loadMinPlain: BigDecimal =
    parseNum("play.json.serializer.minPlain", defaultMinPlain)(BigDecimal.exact)

  private[json] def loadMaxPlain: BigDecimal =
    parseNum("play.json.serializer.maxPlain", defaultMaxPlain)(BigDecimal.exact)

  private[json] def loadPreserveZeroDecimal: Boolean =
    parseNum("play.json.serializer.preserveZeroDecimal", defaultPreserveZeroDecimal)(_.toBoolean)

  val settings: JsonConfig =
    JsonConfig(
      BigDecimalParseConfig(loadMathContext, loadScaleLimit, loadDigitsLimit),
      BigDecimalSerializerConfig(loadMinPlain, loadMaxPlain, loadPreserveZeroDecimal)
    )

  def apply(): JsonConfig = apply(BigDecimalParseConfig(), BigDecimalSerializerConfig())

  def apply(
      bigDecimalParseConfig: BigDecimalParseConfig,
      bigDecimalSerializerConfig: BigDecimalSerializerConfig
  ): JsonConfig =
    JsonConfigImpl(bigDecimalParseConfig, bigDecimalSerializerConfig)

  private[json] def parseMathContext(key: String): MathContext = sys.props.get(key).map(_.toLowerCase) match {
    case Some("decimal128") => MathContext.DECIMAL128
    case Some("decimal64")  => MathContext.DECIMAL64
    case Some("decimal32")  => MathContext.DECIMAL32
    case Some("unlimited")  => MathContext.UNLIMITED
    case _                  => defaultMathContext
  }

  private[json] def parseNum[T](key: String, default: T)(f: String => T): T =
    try {
      sys.props.get(key).map(f).getOrElse(default)
    } catch {
      case NonFatal(_) => default
    }
}

private final case class JsonConfigImpl(
    bigDecimalParseConfig: BigDecimalParseConfig,
    bigDecimalSerializerConfig: BigDecimalSerializerConfig
) extends JsonConfig

@deprecated("Use BigDecimalParseConfig instead", "2.10.0")
final case class BigDecimalParseSettings(
    mathContext: MathContext = MathContext.DECIMAL128,
    scaleLimit: Int,
    digitsLimit: Int
) extends BigDecimalParseConfig

@deprecated("Use BigDecimalSerializerConfig instead", "2.10.0")
final case class BigDecimalSerializerSettings(
    minPlain: BigDecimal,
    maxPlain: BigDecimal
) extends BigDecimalSerializerConfig {
  override def preserveZeroDecimal: Boolean = defaultPreserveZeroDecimal
}

@deprecated("Use JsonConfig instead", "2.10.0")
final case class JsonParserSettings(
    bigDecimalParseSettings: BigDecimalParseSettings,
    bigDecimalSerializerSettings: BigDecimalSerializerSettings
) extends JsonConfig {
  override def bigDecimalParseConfig: BigDecimalParseConfig = bigDecimalParseSettings

  override def bigDecimalSerializerConfig: BigDecimalSerializerConfig = bigDecimalSerializerSettings
}

object JsonParserSettings {
  val defaultMathContext: MathContext = JsonConfig.defaultMathContext

  // Limit for the scale considering the MathContext of 128
  // limit for scale for decimal128: BigDecimal("0." + "0" * 33 + "1e-6143", java.math.MathContext.DECIMAL128).scale + 1
  val defaultScaleLimit: Int = JsonConfig.defaultScaleLimit

  // 307 digits should be the correct value for 128 bytes. But we are using 310
  // because Play JSON uses BigDecimal to parse any number including Doubles and
  // Doubles max value has 309 digits, so we are using 310 here
  val defaultDigitsLimit: Int = JsonConfig.defaultDigitsLimit

  // Maximum magnitude of BigDecimal to write out as a plain string
  val MaxPlain: BigDecimal = JsonConfig.defaultMaxPlain

  // Minimum magnitude of BigDecimal to write out as a plain string
  val MinPlain: BigDecimal = JsonConfig.defaultMinPlain

  def apply(): JsonParserSettings = JsonParserSettings(
    BigDecimalParseSettings(defaultMathContext, defaultScaleLimit, defaultDigitsLimit),
    BigDecimalSerializerSettings(minPlain = MinPlain, maxPlain = MaxPlain)
  )

  /**
   * Return the default settings configured from System properties.
   */
  val settings: JsonParserSettings = {
    JsonParserSettings(
      BigDecimalParseSettings(loadMathContext, loadScaleLimit, loadDigitsLimit),
      BigDecimalSerializerSettings(loadMinPlain, loadMaxPlain)
    )
  }
}
