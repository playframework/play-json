/*
 * Copyright (C) 2009-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import play.api.libs.json.Json._

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class JsonSpec extends AnyWordSpec with Matchers {
  "Complete JSON should create full object" when {
    "maintain precision when parsing BigDecimals within precision limit" in {
      val n = BigDecimal("12345678901234567890.123456789")

      parse(stringify(toJson(n))).mustEqual(
        JsNumber(
          n
        )
      )
    }

    "lose precision when parsing big integers" in {
      // By big integers, we just mean integers that overflow long,
      // since Jackson has different code paths for them from decimals
      val json = toJson(BigDecimal("1.2345678901234568E+29"))
      parse(stringify(json)).mustEqual(json)
    }

    "keep similar object between serialized and deserialized data".taggedAs(UnstableInScala213) in {
      val original = Json.obj(
        "key1" -> "value1",
        "key2" -> true,
        "key3" -> JsNull,
        "key4" -> Json.arr(1, 2.5, "value2", false, JsNull),
        "key5" -> Json.obj(
          "key6" -> "こんにちは",
          "key7" -> BigDecimal("12345678901234567000")
        )
      )
      val originalString = Json.stringify(original)
      val parsed         = Json.parse(originalString)

      parsed.asInstanceOf[JsObject].fields.mustEqual(original.fields)
      Json.stringify(parsed).mustEqual(originalString)
    }

    "parse from InputStream" in {
      val js = Json.obj(
        "key1" -> "value1",
        "key2" -> true,
        "key3" -> JsNull,
        "key4" -> Json.arr(1, 2.5, "value2", false, JsNull),
        "key5" -> Json.obj(
          "key6" -> "こんにちは",
          "key7" -> BigDecimal("12345678901234567890.123456789")
        )
      )
      def stream = new java.io.ByteArrayInputStream(
        js.toString.getBytes("UTF-8")
      )

      Json.parse(stream).mustEqual(js)
    }
  }
}
