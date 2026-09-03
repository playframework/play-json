/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import com.fasterxml.jackson.core.{ StreamReadConstraints, StreamWriteConstraints }

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

final class JsonConfigSpec extends AnyWordSpec with Matchers {
  "JsonConfig" should {
    "fetch default setting".which {
      "is defined for nesting depth (parsing)" in {
        JsonConfig.defaultStreamReadConstraints.getMaxNestingDepth.
          mustEqual(StreamReadConstraints.DEFAULT_MAX_DEPTH)
      }

      "is defined for nesting depth (serializer)" in {
        JsonConfig.defaultStreamWriteConstraints.getMaxNestingDepth.
          mustEqual(StreamWriteConstraints.DEFAULT_MAX_DEPTH)
      }

      "is defined for Jackson BigDecimal parser" in {
        JsonConfig.loadUseJacksonBigDecimalParser mustEqual false

        JsonConfig.settings.bigDecimalParseConfig.useJacksonBigDecimalParser mustEqual false

        JsonConfig.loadUseJacksonBigDecimalFastParser mustEqual false

        JsonConfig.settings.bigDecimalParseConfig.
          useJacksonBigDecimalFastParser mustEqual false

      }
    }

    "override nesting depth (parsing)" in withProperty(JsonConfig.maxNestingDepth, "200") {
      JsonConfig.loadMaxNestingDepth mustEqual 200
    }

    "override nesting depth (serializer)" in withProperty(
      JsonConfig.maxSerializerNestingDepth, "300") {

      JsonConfig.loadMaxSerializerNestingDepth mustEqual 300
    }

    "override use of Jackson BigDecimal parser" in withProperty(
      "play.json.parser.useJacksonBigDecimalParser", "true") {
      JsonConfig.loadUseJacksonBigDecimalParser mustEqual true
    }

    "override the use of Jackson BigDecimal fast parser" in withProperty(
      "play.json.parser.useJacksonBigDecimalFastParser", "true") {
      JsonConfig.loadUseJacksonBigDecimalFastParser mustEqual true
    }
  }

  // ---

  private def withProperty[T](key: String, value: String)(f: => T): T = {
    System.setProperty(key, value)

    try {
      f
    } finally {
      System.clearProperty(key)
    }
  }
}
