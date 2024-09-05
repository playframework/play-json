/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import com.fasterxml.jackson.core.StreamReadConstraints

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
 * Parse and serialization settings for BigDecimals. Defines limits that will be used when parsing the BigDecimals,
 * like how many digits are accepted.
 */
sealed trait BigDecimalParseConfig {

  /**
   * The [[MathContext]] used when parsing, which will be "decimal32", "decimal64", "decimal128" (default),
   * or "unlimited".
   * This can be set using the [[JsonConfig.mathContextProperty]] system property.
   */
  def mathContext: MathContext

  /**
   * Limits the scale, and it is related to the math context used.
   * The default value is [[JsonConfig.defaultScaleLimit]].
   * This can be set using the [[JsonConfig.scaleLimitProperty]] system property.
   */
  def scaleLimit: Int

  /**
   * How many digits are accepted, also related to the math context used.
   * The default value is [[JsonConfig.defaultDigitsLimit]].
   * This can be set using the [[JsonConfig.digitsLimitProperty]] system property.
   */
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

  /**
   * Minimum magnitude of BigDecimal to write out as a plain string.
   * Defaults to [[JsonConfig.defaultMinPlain]].
   * This can be set using the [[JsonConfig.minPlainProperty]] system property.
   */
  def minPlain: BigDecimal

  /**
   * Maximum magnitude of BigDecimal to write out as a plain string.
   * Defaults to [[JsonConfig.defaultMaxPlain]].
   * This can be set using the [[JsonConfig.maxPlainProperty]] system property.
   */
  def maxPlain: BigDecimal

  /**
   * True to preserve a zero decimal , or false to drop them (the default).
   * For example, 1.00 will be serialized as 1 if false or 1.0 if true (only a single zero is preserved).
   * Other trailing zeroes will be dropped regardless of this value.
   * For example, 1.1000 will always be serialized as 1.1.
   * This can be set using the [[JsonConfig.preserveZeroDecimalProperty]] system property.
   */
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
  def streamReadConstraints: StreamReadConstraints
}

object JsonConfig {

  /**
   * The default math context ("decimal128").
   */
  val defaultMathContext: MathContext = MathContext.DECIMAL128

  /**
   * The default limit for the scale considering the default MathContext of decimal128.
   * limit for scale for decimal128: BigDecimal("0." + "0" * 33 + "1e-6143", java.math.MathContext.DECIMAL128).scale + 1
   */
  val defaultScaleLimit: Int = 6178

  /**
   * The default limit for digits considering the default MathContext of decimal128.
   * 307 digits should be the correct value for 128 bytes. But we are using 310
   * because Play JSON uses BigDecimal to parse any number including Doubles and
   * Doubles max value has 309 digits, so we are using 310 here
   */
  val defaultDigitsLimit: Int = 310

  /**
   * Zero decimal values (e.g. .0 or .00) or dropped by default.
   * For example, a value of 1.0 or 1.00 will be serialized as 1.
   */
  val defaultPreserveZeroDecimal: Boolean = false

  /**
   * The default maximum magnitude of BigDecimal to write out as a plain string.
   */
  val defaultMaxPlain: BigDecimal = 1E20

  /**
   * The default minimum magnitude of BigDecimal to write out as a plain string.
   */
  val defaultMinPlain: BigDecimal = 1E-10

  /**
   * The system property to override the scale limit.
   */
  val scaleLimitProperty: String = "play.json.parser.scaleLimit"

  /**
   * The system property to override the digits limit
   */
  val digitsLimitProperty: String = "play.json.parser.digitsLimit"

  /**
   * The system property to override the math context. This can be "decimal32", "decimal64", "decimal128" (the default),
   * or "unlimited".
   */
  val mathContextProperty: String = "play.json.parser.mathContext"

  /**
   * The system property to override the minimum magnitude of BigDecimal to write out as a plain string
   */
  val minPlainProperty: String = "play.json.serializer.minPlain"

  /**
   * The system property to override the maximum magnitude of BigDecimal to write out as a plain string
   */
  val maxPlainProperty: String = "play.json.serializer.maxPlain"

  /**
   * The system property to override the max nesting depth for JSON parsing.
   */
  val maxNestingDepth: String = "play.json.parser.maxNestingDepth"

  /**
   * The system property to override whether zero decimals (e.g. .0 or .00) are written by default. These are dropped by default.
   */
  val preserveZeroDecimalProperty: String = "play.json.serializer.preserveZeroDecimal"

  private[json] def loadScaleLimit: Int = prop(scaleLimitProperty, defaultScaleLimit)(_.toInt)

  private[json] def loadDigitsLimit: Int = prop(digitsLimitProperty, defaultDigitsLimit)(_.toInt)

  private[json] def loadMathContext: MathContext = parseMathContext(mathContextProperty)

  private[json] def loadMinPlain: BigDecimal = prop(minPlainProperty, defaultMinPlain)(BigDecimal.exact)

  private[json] def loadMaxPlain: BigDecimal = prop(maxPlainProperty, defaultMaxPlain)(BigDecimal.exact)

  private[json] def loadMaxNestingDepth: Int =
    prop(maxNestingDepth, StreamReadConstraints.DEFAULT_MAX_DEPTH)(Integer.parseInt)

  private[json] def loadPreserveZeroDecimal: Boolean =
    prop(preserveZeroDecimalProperty, defaultPreserveZeroDecimal)(_.toBoolean)

  private[json] val defaultStreamReadConstraints: StreamReadConstraints =
    StreamReadConstraints
      .builder()
      .maxNestingDepth(loadMaxNestingDepth)
      .maxNumberLength(Int.MaxValue) // play-json has its own support for limiting number length
      .build()

  // Default settings, which can be controlled with system properties.
  // To override, call JacksonJson.setConfig()
  val settings: JsonConfig =
    JsonConfig(
      BigDecimalParseConfig(loadMathContext, loadScaleLimit, loadDigitsLimit),
      BigDecimalSerializerConfig(loadMinPlain, loadMaxPlain, loadPreserveZeroDecimal),
      defaultStreamReadConstraints
    )

  def apply(): JsonConfig = apply(BigDecimalParseConfig(), BigDecimalSerializerConfig())

  def apply(
      bigDecimalParseConfig: BigDecimalParseConfig,
      bigDecimalSerializerConfig: BigDecimalSerializerConfig
  ): JsonConfig =
    JsonConfigImpl(bigDecimalParseConfig, bigDecimalSerializerConfig, defaultStreamReadConstraints)

  def apply(
      bigDecimalParseConfig: BigDecimalParseConfig,
      bigDecimalSerializerConfig: BigDecimalSerializerConfig,
      streamReadConstraints: StreamReadConstraints
  ): JsonConfig =
    JsonConfigImpl(bigDecimalParseConfig, bigDecimalSerializerConfig, streamReadConstraints)

  private[json] def parseMathContext(key: String): MathContext = sys.props.get(key).map(_.toLowerCase) match {
    case Some("decimal128") => MathContext.DECIMAL128
    case Some("decimal64")  => MathContext.DECIMAL64
    case Some("decimal32")  => MathContext.DECIMAL32
    case Some("unlimited")  => MathContext.UNLIMITED
    case _                  => defaultMathContext
  }

  private[json] def prop[T](key: String, default: T)(f: String => T): T =
    try {
      sys.props.get(key).map(f).getOrElse(default)
    } catch {
      case NonFatal(_) => default
    }
}

private final case class JsonConfigImpl(
    bigDecimalParseConfig: BigDecimalParseConfig,
    bigDecimalSerializerConfig: BigDecimalSerializerConfig,
    streamReadConstraints: StreamReadConstraints
) extends JsonConfig

@deprecated("Use BigDecimalParseConfig instead", "2.9.4")
final case class BigDecimalParseSettings(
    mathContext: MathContext = MathContext.DECIMAL128,
    scaleLimit: Int,
    digitsLimit: Int
) extends BigDecimalParseConfig

@deprecated("Use BigDecimalSerializerConfig instead", "2.9.4")
final case class BigDecimalSerializerSettings(
    minPlain: BigDecimal,
    maxPlain: BigDecimal
) extends BigDecimalSerializerConfig {
  override def preserveZeroDecimal: Boolean = defaultPreserveZeroDecimal
}

@deprecated("Use JsonConfig instead", "2.9.4")
final case class JsonParserSettings(
    bigDecimalParseSettings: BigDecimalParseSettings,
    bigDecimalSerializerSettings: BigDecimalSerializerSettings,
    streamReadConstraints: StreamReadConstraints = JsonConfig.defaultStreamReadConstraints
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
