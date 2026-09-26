/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import java.io.{ ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream }

final class JsonSerializationSpec extends org.specs2.mutable.Specification {
  "Serialization".title

  "Object" should {
    "be serialized/deserialized correctly using Java serialization" in {
      val originalObj = Json.obj(
        "field1" -> 123,
        "field2" -> "abc",
        "field3" -> JsNull,
        "obj"    -> Json.obj("field1" -> 234),
        "arr"    -> JsArray(
          Seq(
            JsString("abc"),
            JsNumber(123),
            JsBoolean(true),
            JsNull,
            Json.obj("field1" -> 345)
          )
        )
      )

      val bos = new ByteArrayOutputStream()
      val out = new ObjectOutputStream(bos)

      out.writeObject(originalObj)

      val bis = new ByteArrayInputStream(bos.toByteArray)
      val in  = new ObjectInputStream(bis)

      in.readObject().asInstanceOf[JsObject].must_==(originalObj)
    }
  }

  "Number" should {
    "be serialized when using integer" >> {
      "up to Int" in {
        val positive = "123"
        val negative = s"-${positive}"

        json(JsNumber(123.toByte)) must_=== positive and {
          json(JsNumber(123.toShort)) must_=== positive
        } and {
          json(JsNumber(123)) must_=== positive
        } and {
          json(JsNumber(-123.toByte)) must_=== negative
        } and {
          json(JsNumber(-123.toShort)) must_=== negative
        } and {
          json(JsNumber(-123)) must_=== negative
        }
      }

      "from Long" in {
        json(JsNumber(Long.MinValue)) must_=== "-9223372036854775808" and {
          json(JsNumber(Long.MaxValue)) must_=== "9223372036854775807"
        }
      }
    }

    "be serialized from BigInt" in {
      json(JsNumber(BigInt(Long.MinValue))) must_=== "-9223372036854775808" and {
        json(JsNumber(BigInt(Long.MaxValue))) must_=== "9223372036854775807"
      }
    }

    "be serialized when using Float" >> {
      "strip trailing zeros from a decimal value" in {
        json(JsNumber(123.45000F)) must_=== "123.44999694824219" // allowed rounding
      }

      "serialize a decimal whose trailing zeros are all stripped as an integer" in {
        json(JsNumber(123.000F)) must_=== "123"
      }
    }

    "be serialized when using BigDecimal" >> {
      "preserve the exact decimal value" in {
        json(JsNumber(BigDecimal("123.456789012345678901234567890"))) mustEqual "123.45678901234567890123456789"
      }

      "strip trailing zeros from a decimal value" in {
        json(JsNumber(BigDecimal("123.45000"))) mustEqual "123.45"
      }

      "serialize a decimal whose trailing zeros are all stripped as an integer" in {
        json(JsNumber(BigDecimal("123.000"))) mustEqual "123"
      }

      "preserve precision for a large plain decimal" in {
        json(
          JsNumber(BigDecimal("123456789012345678901234567890.123456789"))
        ) mustEqual "123456789012345678901234567890.123456789"
      }

      "use plain notation for a large value within the plain range" in {
        json(JsNumber(BigDecimal("1.234567890123456789E+10"))) mustEqual "12345678901.23456789"
      }

      "use scientific notation for a value outside the maximum plain range" in {
        val value = BigDecimal("1E+1000")

        json(JsNumber(value)) mustEqual "1E+1000"
      }

      "use plain notation for a small value within the plain range" in {
        json(JsNumber(BigDecimal("1.23456789E-5"))) mustEqual "0.0000123456789"
      }

      "use scientific notation for a value outside the minimum plain range" in {
        val value = BigDecimal("1E-1000")

        json(JsNumber(value)) mustEqual "1E-1000"
      }

      "preserve a high-precision value when scientific notation is used" in {
        json(
          JsNumber(BigDecimal("1.23456789012345678901234567890123456789E+1000"))
        ) mustEqual "1.23456789012345678901234567890123456789E+1000"
      }

      "preserve a negative high-precision value" in {
        json(
          JsNumber(BigDecimal("-1.23456789012345678901234567890123456789E+1000"))
        ) mustEqual "-1.23456789012345678901234567890123456789E+1000"
      }

      "serialize zero without a decimal part" in {
        json(JsNumber(BigDecimal("0.0000"))) mustEqual "0"
      }
    }

    "be serialized as-is from parsed number" >> {
      "for integer" in {
        val n = new JsNumber.JsLazy(JsNumber.NumberType.Integer, "123", BigDecimal("123"))

        json(n) must_=== "123"
      }

      "for decimal" in {
        val n = new JsNumber.JsLazy(JsNumber.NumberType.Float, "-1.0e10", BigDecimal("-1.0e10"))

        json(n) must_=== "-1.0e10"
      }
    }
  }

  // ---

  private def json(js: JsValue): String = new String(Json.toBytes(js), java.nio.charset.StandardCharsets.UTF_8)
}
