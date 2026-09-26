/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import java.util.Locale
import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._

/**
 * Benchmark for the Locale OWrites implementation.
 *
 * Cases cover:
 *   - language only
 *   - language + country
 *   - language + country + variant
 *   - language + script
 *   - Unicode attributes
 *   - Unicode keywords
 *   - extensions
 *   - all supported fields
 *
 * Locale instances are created once during benchmark setup so that
 * Locale construction is not included in the measured operations.
 */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
class LocaleWritesBench {
  import Writes.localeObjectWrites

  private val languageOnlyLocale =
    new Locale("en")

  private val countryLocale =
    new Locale("en", "US")

  private val variantLocale =
    new Locale("en", "US", "POSIX")

  private val scriptLocale =
    new Locale.Builder()
      .setLanguage("en")
      .setScript("Latn")
      .build()

  private val attributesLocale =
    new Locale.Builder()
      .setLanguage("en")
      .addUnicodeLocaleAttribute("foo")
      .addUnicodeLocaleAttribute("bar")
      .build()

  private val keywordsLocale =
    new Locale.Builder()
      .setLanguage("en")
      .setUnicodeLocaleKeyword("ca", "gregory")
      .setUnicodeLocaleKeyword("nu", "latn")
      .setUnicodeLocaleKeyword("co", "phonebk")
      .build()

  private val extensionLocale =
    new Locale.Builder()
      .setLanguage("en")
      .setExtension('x', "private")
      .build()

  private val allLocale =
    new Locale.Builder()
      .setLanguage("en")
      .setRegion("US")
      .setScript("Latn")
      .setVariant("POSIX")
      .addUnicodeLocaleAttribute("foo")
      .addUnicodeLocaleAttribute("bar")
      .setUnicodeLocaleKeyword("ca", "gregory")
      .setUnicodeLocaleKeyword("nu", "latn")
      .setUnicodeLocaleKeyword("co", "phonebk")
      .setExtension('x', "private")
      .build()

  // ---

  @Benchmark
  def languageOnly(): JsObject =
    localeObjectWrites.writes(languageOnlyLocale)

  @Benchmark
  def country(): JsObject =
    localeObjectWrites.writes(countryLocale)

  @Benchmark
  def variant(): JsObject =
    localeObjectWrites.writes(variantLocale)

  @Benchmark
  def script(): JsObject =
    localeObjectWrites.writes(scriptLocale)

  @Benchmark
  def attributes(): JsObject =
    localeObjectWrites.writes(attributesLocale)

  @Benchmark
  def keywords(): JsObject =
    localeObjectWrites.writes(keywordsLocale)

  @Benchmark
  def extension(): JsObject =
    localeObjectWrites.writes(extensionLocale)

  @Benchmark
  def all(): JsObject =
    localeObjectWrites.writes(allLocale)
}
