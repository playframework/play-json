/*
 * Copyright (C) 2009-2018 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import java.math.MathContext
import java.util.concurrent.atomic.AtomicReference

/**
 * Parse settings for BigDecimals. Defines limits that will be used when parsing the BigDecimals, like how many digits
 * are accepted.
 *
 * @param mathContext the [[MathContext]] used when parsing.
 * @param scaleLimit limit the scale, and it is related to the math context used.
 * @param digitsLimit how many digits are accepted, also related to the math context used.
 */
case class BigDecimalParseSettings(
  mathContext: MathContext = MathContext.DECIMAL128,
  // Limit for the scale considering the MathContext of 128
  // limit for scale for decimal128: BigDecimal("0." + "0" * 33 + "1e-6143", java.math.MathContext.DECIMAL128).scale + 1
  scaleLimit: Int = 6178,
  // 307 digits should be the correct value for 128 bytes. But we are using 310
  // because Play JSON uses BigDecimal to parse any number including Doubles and
  // Doubles max value has 309 digits, so we are using 310 here
  digitsLimit: Int = 310
)

case class JsonParserSettings(bigDecimalParseSettings: BigDecimalParseSettings = BigDecimalParseSettings())

object JsonParserSettings {

  // Initialize with the default settings. Since most of things happening in play-json are
  // global, we are using a AtomicReference here. Also notice that these settings should be
  // changed at application startup and it is not possible yet to re-configured after the
  // parse is used.
  //
  // So, in a Play application, this will be usually configured either using an eager singleton
  // in in an ApplicationLoader. Other frameworks using play-json should provide their own ways
  // to do the initialization.
  private val defaultSettings = new AtomicReference[JsonParserSettings](JsonParserSettings())

  /**
   * Configure the parse settings.
   *
   * @param jsonParserSettings the parse settings that will be used.
   */
  def configure(jsonParserSettings: JsonParserSettings): Unit = {
    defaultSettings.set(jsonParserSettings)
  }

  /**
   * Return the parse settings that are configured.
   */
  def settings: JsonParserSettings = defaultSettings.get()
}