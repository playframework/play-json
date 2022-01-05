/*
 * Copyright (C) 2009-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import java.util.Locale

object LocaleFixtures {

  def fullLocale =
    new Locale.Builder()
      .setLocale(Locale.FRANCE)
      .addUnicodeLocaleAttribute("foo")
      .addUnicodeLocaleAttribute("bar")
      .setExtension('a', "foo")
      .setExtension('b', "bar")
      .setRegion("FR")
      .setScript("Latn")
      .setVariant("polyton")
      .setUnicodeLocaleKeyword("ka", "ipsum")
      .setUnicodeLocaleKeyword("kb", "value")
      .build()

  val locales = Seq(Locale.FRANCE, Locale.CANADA_FRENCH, new Locale("fr"), fullLocale)

  val tags = Seq("fr-FR", "fr-CA", "fr", "fr-Latn-FR-polyton-a-foo-b-bar-u-bar-foo-ka-ipsum-kb-value")

  val objs = Seq(
    Json.obj("language" -> "fr", "country" -> "FR"),
    Json.obj("language" -> "fr", "country" -> "CA"),
    Json.obj("language" -> "fr"),
    Json.obj(
      "variant"    -> "polyton",
      "country"    -> "FR",
      "attributes" -> Json.arr("bar", "foo"),
      "language"   -> "fr",
      "keywords"   -> Json.obj("ka" -> "ipsum", "kb" -> "value"),
      "script"     -> "Latn",
      "extension" -> Json.obj(
        "a" -> "foo",
        "b" -> "bar",
        "u" -> "bar-foo-ka-ipsum-kb-value"
      )
    )
  )
}
