/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

final class JsNumberExtractorsSpec extends AnyWordSpec with Matchers {
  import JsNumber.{ JsLazy, NumberType }

  "Short" should {
    import JsNumber.ValidShort.{ unapply => extract }

    "be extracted" when {
      "given JsNumericInt" in {
        extract(JsNumber(123)) mustBe Some(123.toShort)
        extract(JsNumber(-123)) mustBe Some((-123).toShort)
        extract(JsNumber(Short.MaxValue)) mustBe Some(Short.MaxValue)
        extract(JsNumber(Short.MinValue)) mustBe Some(Short.MinValue)
        extract(JsNumber(Int.MaxValue)) mustBe None
        extract(JsNumber(Int.MinValue)) mustBe None
      }

      "given JsLong" in {
        extract(JsNumber(123L)) mustBe Some(123.toShort)
        extract(JsNumber(-123L)) mustBe Some((-123).toShort)
        extract(JsNumber(Short.MaxValue.toLong)) mustBe Some(Short.MaxValue)
        extract(JsNumber(Short.MinValue.toLong)) mustBe Some(Short.MinValue)
        extract(JsNumber(Int.MaxValue.toLong + 1)) mustBe None
        extract(JsNumber(Int.MinValue.toLong - 1)) mustBe None
      }

      "given JsBigInteger" in {
        extract(JsNumber(BigInt(123))) mustBe Some(123.toShort)
        extract(JsNumber(BigInt(-123))) mustBe Some((-123).toShort)
        extract(JsNumber(BigInt(Short.MaxValue))) mustBe Some(Short.MaxValue)
        extract(JsNumber(BigInt(Short.MinValue))) mustBe Some(Short.MinValue)
        extract(JsNumber(BigInt(Short.MaxValue) + 1)) mustBe None
        extract(JsNumber(BigInt(Short.MinValue) - 1)) mustBe None
      }

      "given JsNumericFloat" in {
        extract(JsNumber(123.0F)) mustBe Some(123.toShort)
        extract(JsNumber(-123.0F)) mustBe Some((-123).toShort)
        extract(JsNumber(Short.MaxValue.toFloat)) mustBe Some(Short.MaxValue)
        extract(JsNumber(Short.MinValue.toFloat)) mustBe Some(Short.MinValue)
        extract(JsNumber(123.5F)) mustBe None
        extract(JsNumber(32768.0F)) mustBe None
      }

      "given JsLazy" in {
        extract(
          new JsLazy(NumberType.Integer, "123", BigDecimal(123))
        ) mustBe Some(123.toShort)

        extract(
          new JsLazy(NumberType.Integer, "-123", BigDecimal(-123))
        ) mustBe Some((-123).toShort)

        extract(
          new JsLazy(NumberType.Integer, "32767", BigDecimal(32767))
        ) mustBe Some(Short.MaxValue)

        extract(
          new JsLazy(NumberType.Integer, "-32768", BigDecimal(-32768))
        ) mustBe Some(Short.MinValue)

        extract(
          new JsLazy(NumberType.Integer, "32768", BigDecimal(32768))
        ) mustBe None

        extract(
          new JsLazy(NumberType.Integer, "-32769", BigDecimal(-32769))
        ) mustBe None

        extract(
          new JsLazy(NumberType.Float, "123.0", BigDecimal("123.0"))
        ) mustBe Some(123.toShort)

        extract(
          new JsLazy(NumberType.Float, "-123.0", BigDecimal("-123.0"))
        ) mustBe Some((-123).toShort)

        extract(
          new JsLazy(NumberType.Float, "123e0", BigDecimal("123e0"))
        ) mustBe Some(123.toShort)

        extract(
          new JsLazy(NumberType.Float, "123.00e2", BigDecimal("123.00e2"))
        ) mustBe Some(12300.toShort)

        extract(
          new JsLazy(NumberType.Float, "123.45e2", BigDecimal("123.45e2"))
        ) mustBe Some(12345.toShort)

        extract(
          new JsLazy(NumberType.Float, "123.5", BigDecimal("123.5"))
        ) mustBe None

        extract(
          new JsLazy(NumberType.Float, "123e-2", BigDecimal("123e-2"))
        ) mustBe None

        extract(
          new JsLazy(NumberType.Float, "32768.0", BigDecimal("32768.0"))
        ) mustBe None

        extract(
          new JsLazy(NumberType.Float, "-32769.0", BigDecimal("-32769.0"))
        ) mustBe None

        extract(
          new JsLazy(
            NumberType.Float,
            "123.0000000000000000001",
            BigDecimal("123.0000000000000000001")
          )
        ) mustBe None
      }

      "given JsBigDecimal" in {
        extract(JsNumber(BigDecimal(123))) mustBe Some(123.toShort)
        extract(JsNumber(BigDecimal(-123))) mustBe Some((-123).toShort)
        extract(JsNumber(BigDecimal(Short.MaxValue))) mustBe Some(Short.MaxValue)
        extract(JsNumber(BigDecimal(Short.MinValue))) mustBe Some(Short.MinValue)
        extract(JsNumber(BigDecimal("123.0"))) mustBe Some(123.toShort)
        extract(JsNumber(BigDecimal("123.5"))) mustBe None
        extract(JsNumber(BigDecimal(32768))) mustBe None
        extract(JsNumber(BigDecimal(-32769))) mustBe None
      }
    }
  }

  "Byte" should {
    import JsNumber.ValidByte.{ unapply => extract }

    "be extracted" when {
      "given JsNumericInt" in {
        extract(JsNumber(123)) mustBe Some(123.toByte)
        extract(JsNumber(-123)) mustBe Some((-123).toByte)
        extract(JsNumber(Byte.MaxValue)) mustBe Some(Byte.MaxValue)
        extract(JsNumber(Byte.MinValue)) mustBe Some(Byte.MinValue)
        extract(JsNumber(Byte.MaxValue + 1)) mustBe None
        extract(JsNumber(Byte.MinValue - 1)) mustBe None
      }

      "given JsLong" in {
        extract(JsNumber(123L)) mustBe Some(123.toByte)
        extract(JsNumber(-123L)) mustBe Some((-123).toByte)
        extract(JsNumber(Byte.MaxValue.toLong)) mustBe Some(Byte.MaxValue)
        extract(JsNumber(Byte.MinValue.toLong)) mustBe Some(Byte.MinValue)
        extract(JsNumber(Byte.MaxValue.toLong + 1)) mustBe None
        extract(JsNumber(Byte.MinValue.toLong - 1)) mustBe None
      }

      "given JsBigInteger" in {
        extract(JsNumber(BigInt(123))) mustBe Some(123.toByte)
        extract(JsNumber(BigInt(-123))) mustBe Some((-123).toByte)
        extract(JsNumber(BigInt(Byte.MaxValue))) mustBe Some(Byte.MaxValue)
        extract(JsNumber(BigInt(Byte.MinValue))) mustBe Some(Byte.MinValue)
        extract(JsNumber(BigInt(Byte.MaxValue) + 1)) mustBe None
        extract(JsNumber(BigInt(Byte.MinValue) - 1)) mustBe None
      }

      "given JsNumericFloat" in {
        extract(JsNumber(123.0F)) mustBe Some(123.toByte)
        extract(JsNumber(-123.0F)) mustBe Some((-123).toByte)
        extract(JsNumber(Byte.MaxValue.toFloat)) mustBe Some(Byte.MaxValue)
        extract(JsNumber(Byte.MinValue.toFloat)) mustBe Some(Byte.MinValue)
        extract(JsNumber(123.5F)) mustBe None
        extract(JsNumber(128.0F)) mustBe None
        extract(JsNumber(-129.0F)) mustBe None
      }

      "given JsLazy" in {
        extract(
          new JsLazy(NumberType.Integer, "123", BigDecimal(123))
        ) mustBe Some(123.toByte)

        extract(
          new JsLazy(NumberType.Integer, "-123", BigDecimal(-123))
        ) mustBe Some((-123).toByte)

        extract(
          new JsLazy(NumberType.Integer, "127", BigDecimal(127))
        ) mustBe Some(Byte.MaxValue)

        extract(
          new JsLazy(NumberType.Integer, "-128", BigDecimal(-128))
        ) mustBe Some(Byte.MinValue)

        extract(
          new JsLazy(NumberType.Integer, "128", BigDecimal(128))
        ) mustBe None

        extract(
          new JsLazy(NumberType.Integer, "-129", BigDecimal(-129))
        ) mustBe None

        extract(
          new JsLazy(NumberType.Float, "123.0", BigDecimal("123.0"))
        ) mustBe Some(123.toByte)

        extract(
          new JsLazy(NumberType.Float, "-123.0", BigDecimal("-123.0"))
        ) mustBe Some((-123).toByte)

        extract(
          new JsLazy(NumberType.Float, "123e0", BigDecimal("123e0"))
        ) mustBe Some(123.toByte)

        extract(
          new JsLazy(NumberType.Float, "12.3e1", BigDecimal("12.3e1"))
        ) mustBe Some(123.toByte)

        extract(
          new JsLazy(NumberType.Float, "123.00", BigDecimal("123.00"))
        ) mustBe Some(123.toByte)

        extract(
          new JsLazy(NumberType.Float, "123.5", BigDecimal("123.5"))
        ) mustBe None

        extract(
          new JsLazy(NumberType.Float, "128.0", BigDecimal("128.0"))
        ) mustBe None

        extract(
          new JsLazy(NumberType.Float, "-129.0", BigDecimal("-129.0"))
        ) mustBe None

        extract(
          new JsLazy(
            NumberType.Float,
            "123.0000000000000000001",
            BigDecimal("123.0000000000000000001")
          )
        ) mustBe None
      }

      "given JsBigDecimal" in {
        extract(JsNumber(BigDecimal(123))) mustBe Some(123.toByte)
        extract(JsNumber(BigDecimal(-123))) mustBe Some((-123).toByte)
        extract(JsNumber(BigDecimal(Byte.MaxValue))) mustBe Some(Byte.MaxValue)
        extract(JsNumber(BigDecimal(Byte.MinValue))) mustBe Some(Byte.MinValue)
        extract(JsNumber(BigDecimal("123.0"))) mustBe Some(123.toByte)
        extract(JsNumber(BigDecimal("123.5"))) mustBe None
        extract(JsNumber(BigDecimal(128))) mustBe None
        extract(JsNumber(BigDecimal(-129))) mustBe None
      }
    }
  }

  "Int" should {
    import JsNumber.ValidInt.{ unapply => extract }

    "be extracted" when {
      "given JsNumericInt" in {
        extract(JsNumber(123)) mustBe Some(123)
        extract(JsNumber(Int.MaxValue)) mustBe Some(Int.MaxValue)
        extract(JsNumber(Int.MinValue)) mustBe Some(Int.MinValue)
      }

      "given JsLong" in {
        extract(JsNumber(123L)) mustBe Some(123)
        extract(JsNumber(Int.MaxValue.toLong)) mustBe Some(Int.MaxValue)
        extract(JsNumber(Int.MinValue.toLong)) mustBe Some(Int.MinValue)
        extract(JsNumber(Int.MaxValue.toLong + 1)) mustBe None
        extract(JsNumber(Int.MinValue.toLong - 1)) mustBe None
      }

      "given JsBigInteger" in {
        extract(JsNumber(BigInt(123))) mustBe Some(123)
        extract(JsNumber(BigInt(Int.MaxValue))) mustBe Some(Int.MaxValue)
        extract(JsNumber(BigInt(Int.MinValue))) mustBe Some(Int.MinValue)
        extract(JsNumber(BigInt(Int.MaxValue) + 1)) mustBe None
        extract(JsNumber(BigInt(Int.MinValue) - 1)) mustBe None
      }

      "given JsNumericFloat" in {
        extract(JsNumber(123.0F)) mustBe Some(123)
        extract(JsNumber(-123.0F)) mustBe Some(-123)
        extract(JsNumber(0.0F)) mustBe Some(0)
        extract(JsNumber(123.5F)) mustBe None
        extract(JsNumber(-123.5F)) mustBe None
        extract(JsNumber(2147483648.0F)) mustBe None
        extract(JsNumber(-2147483648.0F)) mustBe Some(Int.MinValue)
      }

      "given JsLazy" in {
        extract(
          new JsLazy(NumberType.Integer, "123", BigDecimal(123))
        ) mustBe Some(123)

        extract(
          new JsLazy(NumberType.Integer, "-123", BigDecimal(-123))
        ) mustBe Some(-123)

        extract(
          new JsLazy(NumberType.Integer, "2147483647", BigDecimal(Int.MaxValue))
        ) mustBe Some(Int.MaxValue)

        extract(
          new JsLazy(NumberType.Integer, "-2147483648", BigDecimal(Int.MinValue))
        ) mustBe Some(Int.MinValue)

        extract(
          new JsLazy(NumberType.Integer, "2147483648", BigDecimal("2147483648"))
        ) mustBe None

        extract(
          new JsLazy(NumberType.Integer, "-2147483649", BigDecimal("-2147483649"))
        ) mustBe None

        extract(
          new JsLazy(NumberType.Float, "123.0", BigDecimal("123.0"))
        ) mustBe Some(123)

        extract(
          new JsLazy(NumberType.Float, "-123.0", BigDecimal("-123.0"))
        ) mustBe Some(-123)

        extract(
          new JsLazy(NumberType.Float, "123e0", BigDecimal("123e0"))
        ) mustBe Some(123)

        extract(
          new JsLazy(NumberType.Float, "123.00e2", BigDecimal("123.00e2"))
        ) mustBe Some(12300)

        extract(
          new JsLazy(NumberType.Float, "123.45e2", BigDecimal("123.45e2"))
        ) mustBe Some(12345)

        extract(
          new JsLazy(NumberType.Float, "123.5", BigDecimal("123.5"))
        ) mustBe None

        extract(
          new JsLazy(NumberType.Float, "123e-2", BigDecimal("123e-2"))
        ) mustBe None

        extract(
          new JsLazy(NumberType.Float, "2147483648.0", BigDecimal("2147483648.0"))
        ) mustBe None

        extract(
          new JsLazy(NumberType.Float, "-2147483649.0", BigDecimal("-2147483649.0"))
        ) mustBe None

        extract(
          new JsLazy(
            NumberType.Float,
            "123.0000000000000000001",
            BigDecimal("123.0000000000000000001")
          )
        ) mustBe None
      }

      "given JsBigDecimal" in {
        extract(JsNumber(BigDecimal(123))) mustBe Some(123)
        extract(JsNumber(BigDecimal(Int.MaxValue))) mustBe Some(Int.MaxValue)
        extract(JsNumber(BigDecimal(Int.MinValue))) mustBe Some(Int.MinValue)
        extract(JsNumber(BigDecimal("123.0"))) mustBe Some(123)
        extract(JsNumber(BigDecimal("123.5"))) mustBe None
        extract(JsNumber(BigDecimal("2147483648"))) mustBe None
        extract(JsNumber(BigDecimal("-2147483649"))) mustBe None
      }
    }
  }

  "Long" should {
    "be extracted" when {
      import JsNumber.ValidLong.{ unapply => extract }

      "given JsNumericInt" in {
        extract(JsNumber(123)) mustBe Some(123L)
        extract(JsNumber(-123)) mustBe Some(-123L)
        extract(JsNumber(Int.MaxValue)) mustBe Some(Int.MaxValue.toLong)
        extract(JsNumber(Int.MinValue)) mustBe Some(Int.MinValue.toLong)
      }

      "given JsLong" in {
        extract(JsNumber(123L)) mustBe Some(123L)
        extract(JsNumber(-123L)) mustBe Some(-123L)
        extract(JsNumber(Long.MaxValue)) mustBe Some(Long.MaxValue)
        extract(JsNumber(Long.MinValue)) mustBe Some(Long.MinValue)
      }

      "given JsBigInteger" in {
        extract(JsNumber(BigInt(123))) mustBe Some(123L)
        extract(JsNumber(BigInt(-123))) mustBe Some(-123L)
        extract(JsNumber(BigInt(Long.MaxValue))) mustBe Some(Long.MaxValue)
        extract(JsNumber(BigInt(Long.MinValue))) mustBe Some(Long.MinValue)
        extract(JsNumber(BigInt(Long.MaxValue) + 1)) mustBe None
        extract(JsNumber(BigInt(Long.MinValue) - 1)) mustBe None
      }

      "given JsNumericFloat" in {
        extract(JsNumber(123.0F)) mustBe Some(123L)
        extract(JsNumber(-123.0F)) mustBe Some(-123L)
        extract(JsNumber(123.5F)) mustBe None
        extract(JsNumber(123.0D)) mustBe Some(123L)
        extract(JsNumber(-123.0D)) mustBe Some(-123L)
        extract(JsNumber(123.5D)) mustBe None
      }

      "given JsLazy" in {
        extract(
          new JsLazy(NumberType.Integer, "123", BigDecimal(123))
        ) mustBe Some(123L)

        extract(
          new JsLazy(NumberType.Integer, "-123", BigDecimal(-123))
        ) mustBe Some(-123L)

        extract(
          new JsLazy(
            NumberType.Integer,
            "9223372036854775807",
            BigDecimal(Long.MaxValue)
          )
        ) mustBe Some(Long.MaxValue)

        extract(
          new JsLazy(
            NumberType.Integer,
            "-9223372036854775808",
            BigDecimal(Long.MinValue)
          )
        ) mustBe Some(Long.MinValue)

        extract(
          new JsLazy(
            NumberType.Integer,
            "9223372036854775808",
            BigDecimal("9223372036854775808")
          )
        ) mustBe None

        extract(
          new JsLazy(
            NumberType.Integer,
            "-9223372036854775809",
            BigDecimal("-9223372036854775809")
          )
        ) mustBe None

        extract(
          new JsLazy(NumberType.Float, "123.0", BigDecimal("123.0"))
        ) mustBe Some(123L)

        extract(
          new JsLazy(NumberType.Float, "-123.0", BigDecimal("-123.0"))
        ) mustBe Some(-123L)

        extract(
          new JsLazy(NumberType.Float, "123e0", BigDecimal("123e0"))
        ) mustBe Some(123L)

        extract(
          new JsLazy(NumberType.Float, "123.00e2", BigDecimal("123.00e2"))
        ) mustBe Some(12300L)

        extract(
          new JsLazy(NumberType.Float, "123.45e2", BigDecimal("123.45e2"))
        ) mustBe Some(12345L)

        extract(
          new JsLazy(NumberType.Float, "123.5", BigDecimal("123.5"))
        ) mustBe None

        extract(
          new JsLazy(NumberType.Float, "123e-2", BigDecimal("123e-2"))
        ) mustBe None

        extract(
          new JsLazy(
            NumberType.Float,
            "123.0000000000000000001",
            BigDecimal("123.0000000000000000001")
          )
        ) mustBe None

        extract(
          new JsLazy(
            NumberType.Float,
            "9223372036854775807.0",
            BigDecimal("9223372036854775807.0")
          )
        ) mustBe Some(Long.MaxValue)

        extract(
          new JsLazy(
            NumberType.Float,
            "9223372036854775808.0",
            BigDecimal("9223372036854775808.0")
          )
        ) mustBe None
      }

      "given JsBigDecimal" in {
        extract(JsNumber(BigDecimal(123))) mustBe Some(123L)
        extract(JsNumber(BigDecimal(-123))) mustBe Some(-123L)
        extract(JsNumber(BigDecimal(Long.MaxValue))) mustBe Some(Long.MaxValue)
        extract(JsNumber(BigDecimal(Long.MinValue))) mustBe Some(Long.MinValue)
        extract(JsNumber(BigDecimal("123.0"))) mustBe Some(123L)
        extract(JsNumber(BigDecimal("123.5"))) mustBe None
        extract(JsNumber(BigDecimal("9223372036854775808"))) mustBe None
        extract(JsNumber(BigDecimal("-9223372036854775809"))) mustBe None
      }
    }
  }

  "Float" should {
    "be extracted" when {
      import JsNumber.ValidFloat.{ unapply => extract }

      "given JsNumericInt" in {
        extract(JsNumber(0)) mustBe Some(0.0F)
        extract(JsNumber(1)) mustBe Some(1.0F)
        extract(JsNumber(-1)) mustBe Some(-1.0F)

        extract(JsNumber(16777215)) mustBe Some(16777215.0F)
        extract(JsNumber(16777216)) mustBe Some(16777216.0F)
        extract(JsNumber(16777217)) mustBe Some(16777217.0F)
        extract(JsNumber(16777218)) mustBe Some(16777218.0F)

        extract(JsNumber(Int.MinValue)) mustBe Some(-2147483648.0F)
        extract(JsNumber(Int.MaxValue)) mustBe Some(2147483648.0F)
      }

      "given JsLong" in {
        extract(JsNumber(0L)) mustBe Some(0.0F)
        extract(JsNumber(1L)) mustBe Some(1.0F)
        extract(JsNumber(-1L)) mustBe Some(-1.0F)

        extract(JsNumber(16777215L)) mustBe Some(16777215.0F)
        extract(JsNumber(16777216L)) mustBe Some(16777216.0F)
        extract(JsNumber(16777217L)) mustBe Some(16777217.0F)
        extract(JsNumber(16777218L)) mustBe Some(16777218.0F)

        extract(JsNumber(33554432L)) mustBe Some(33554432.0F)
        extract(JsNumber(33554433L)) mustBe Some(33554433.0F)
        extract(JsNumber(33554434L)) mustBe Some(33554434.0F)
        extract(JsNumber(33554436L)) mustBe Some(33554436.0F)

        extract(JsNumber(Long.MaxValue)) mustBe Some(9223372036854776000.0F)
        extract(JsNumber(Long.MinValue)) mustBe Some(Long.MinValue.toFloat)
      }

      "given JsBigInteger" in {
        extract(JsNumber(BigInt(0))) mustBe Some(0.0F)
        extract(JsNumber(BigInt(1))) mustBe Some(1.0F)
        extract(JsNumber(BigInt(-1))) mustBe Some(-1.0F)

        extract(JsNumber(BigInt(16777215))) mustBe Some(16777215.0F)
        extract(JsNumber(BigInt(16777216))) mustBe Some(16777216.0F)
        extract(JsNumber(BigInt(16777217))) mustBe None
        extract(JsNumber(BigInt(16777218))) mustBe Some(16777218.0F)

        extract(JsNumber(BigInt(33554432))) mustBe Some(33554432.0F)
        extract(JsNumber(BigInt(33554433))) mustBe None

        extract(JsNumber(BigInt(Long.MinValue))) mustBe Some(Long.MinValue.toFloat)
        extract(JsNumber(BigInt(Long.MaxValue))) mustBe None
      }

      "given JsNumericFloat" in {
        extract(JsNumber(0.0F)) mustBe Some(0.0F)
        extract(JsNumber(-0.0F)) mustBe Some(-0.0F)
        extract(JsNumber(1.0F)) mustBe Some(1.0F)
        extract(JsNumber(-1.0F)) mustBe Some(-1.0F)
        extract(JsNumber(1.5F)) mustBe Some(1.5F)
        extract(JsNumber(-1.5F)) mustBe Some(-1.5F)
        extract(JsNumber(123.5F)) mustBe Some(123.5F)

        extract(JsNumber(Float.MaxValue)) mustBe Some(Float.MaxValue)
        extract(JsNumber(-Float.MaxValue)) mustBe Some(-Float.MaxValue)
        extract(JsNumber(Float.MinPositiveValue)) mustBe Some(Float.MinPositiveValue)
        extract(JsNumber(-Float.MinPositiveValue)) mustBe Some(-Float.MinPositiveValue)

        extract(JsNumber(-1.0D)) mustBe Some(-1.0F)
        extract(JsNumber(-0.0D)) mustBe Some(-0.0F)
        extract(JsNumber(0.0D)) mustBe Some(0.0F)
        extract(JsNumber(0.1D)) mustBe Some(0.1F)
        extract(JsNumber(1.0D)) mustBe Some(1.0F)
        extract(JsNumber(1.1D)) mustBe Some(1.1F)
        extract(JsNumber(1.5D)) mustBe Some(1.5F)
      }

      "given JsLazy integer" in {
        extract(
          new JsLazy(NumberType.Integer, "0", BigDecimal(0))
        ) mustBe Some(0.0F)

        extract(
          new JsLazy(NumberType.Integer, "1", BigDecimal(1))
        ) mustBe Some(1.0F)

        extract(
          new JsLazy(NumberType.Integer, "-1", BigDecimal(-1))
        ) mustBe Some(-1.0F)

        extract(
          new JsLazy(NumberType.Integer, "16777216", BigDecimal(16777216))
        ) mustBe Some(16777216.0F)

        extract(
          new JsLazy(NumberType.Integer, "16777217", BigDecimal(16777217))
        ) mustBe Some(16777217.0F)

        extract(
          new JsLazy(NumberType.Integer, "16777218", BigDecimal(16777218))
        ) mustBe Some(16777218.0F)

        extract(
          new JsLazy(NumberType.Integer, "-16777216", BigDecimal(-16777216))
        ) mustBe Some(-16777216.0F)

        extract(
          new JsLazy(NumberType.Integer, "-16777217", BigDecimal(-16777217))
        ) mustBe Some(-16777217.0F)

        extract(
          new JsLazy(NumberType.Integer, Int.MinValue.toString, BigDecimal(Int.MinValue))
        ) mustBe Some(Int.MinValue.toFloat)
      }

      "given JsLazy float" in {
        extract(
          new JsLazy(NumberType.Float, "0.0", BigDecimal("0.0"))
        ) mustBe Some(0.0F)

        extract(
          new JsLazy(NumberType.Float, "-0.0", BigDecimal("-0.0"))
        ) mustBe Some(-0.0F)

        extract(
          new JsLazy(NumberType.Float, "1.0", BigDecimal("1.0"))
        ) mustBe Some(1.0F)

        extract(
          new JsLazy(NumberType.Float, "-1.0", BigDecimal("-1.0"))
        ) mustBe Some(-1.0F)

        extract(
          new JsLazy(NumberType.Float, "1.5", BigDecimal("1.5"))
        ) mustBe Some(1.5F)

        extract(
          new JsLazy(NumberType.Float, "-1.5", BigDecimal("-1.5"))
        ) mustBe Some(-1.5F)

        extract(
          new JsLazy(NumberType.Float, "16777216.0", BigDecimal("16777216.0"))
        ) mustBe Some(16777216.0F)

        extract(
          new JsLazy(NumberType.Float, "16777217.0", BigDecimal("16777217.0"))
        ) mustBe Some(16777217.0F)

        extract(
          new JsLazy(NumberType.Float, "1e0", BigDecimal("1e0"))
        ) mustBe Some(1.0F)

        extract(
          new JsLazy(NumberType.Float, "15e-1", BigDecimal("15e-1"))
        ) mustBe Some(1.5F)

        extract(
          new JsLazy(NumberType.Float, "0.1", BigDecimal("0.1"))
        ) mustBe Some(0.1F)

        extract(
          new JsLazy(NumberType.Float, "1.1", BigDecimal("1.1"))
        ) mustBe Some(1.1F)

        extract(
          new JsLazy(NumberType.Float, "1e-50", BigDecimal("1e-50"))
        ) mustBe Some(0.0F) // Existing issue/no regression; See https://scastie.scala-lang.org/EzFOL8IyQ3KGntmTW6V5vA

        extract(
          new JsLazy(NumberType.Float, "-1e-50", BigDecimal("-1e-50"))
        ) mustBe Some(-0.0F) // Existing issue/no regression; See https://scastie.scala-lang.org/g0SS5MjhQO2UzTZvbRrgsA

        extract(
          new JsLazy(NumberType.Float, "1e39", BigDecimal("1e39"))
        ) mustBe Some(
          Float.PositiveInfinity
        ) // Existing issue/no regression; See https://scastie.scala-lang.org/vFhCi4yUQS2hojaTygr5uA

        extract(
          new JsLazy(NumberType.Float, "-1e39", BigDecimal("-1e39"))
        ) mustBe Some(
          Float.NegativeInfinity
        ) // Existing issue/no regression; See https://scastie.scala-lang.org/jiqDzAvCSES9bVHvkeAnFA
      }

      "given JsBigDecimal" in {
        extract(JsNumber(BigDecimal(0))) mustBe Some(0.0F)
        extract(JsNumber(BigDecimal(1))) mustBe Some(1.0F)
        extract(JsNumber(BigDecimal(-1))) mustBe Some(-1.0F)

        extract(JsNumber(BigDecimal("1.5"))) mustBe Some(1.5F)
        extract(JsNumber(BigDecimal("-1.5"))) mustBe Some(-1.5F)

        extract(JsNumber(BigDecimal("0.1"))) mustBe Some(0.1F)
        extract(JsNumber(BigDecimal("1.1"))) mustBe Some(1.1F)

        extract(JsNumber(BigDecimal("16777216"))) mustBe Some(16777216.0F)
        extract(JsNumber(BigDecimal("16777217"))) mustBe Some(16777217.0F)
        extract(JsNumber(BigDecimal("16777218"))) mustBe Some(16777218.0F)

        extract(JsNumber(BigDecimal(Long.MinValue))) mustBe Some(BigDecimal(Long.MinValue).toFloat)
        extract(JsNumber(BigDecimal(Long.MaxValue))) mustBe Some(9223372036854775807.0F)
      }
    }
  }

  "Double" should {
    "be extracted" when {
      import JsNumber.ValidDouble.{ unapply => extract }

      "given JsNumericInt" in {
        extract(JsNumber(0)) mustBe Some(0.0D)
        extract(JsNumber(1)) mustBe Some(1.0D)
        extract(JsNumber(-1)) mustBe Some(-1.0D)

        extract(JsNumber(16777215)) mustBe Some(16777215.0D)
        extract(JsNumber(16777216)) mustBe Some(16777216.0D)
        extract(JsNumber(16777218)) mustBe Some(16777218.0D)

        extract(JsNumber(Int.MaxValue)) mustBe Some(Int.MaxValue.toDouble)
        extract(JsNumber(Int.MinValue)) mustBe Some(Int.MinValue.toDouble)
        extract(JsNumber(Int.MinValue + 1)) mustBe Some((Int.MinValue + 1).toDouble)
      }

      "given JsLong" in {
        extract(JsNumber(0L)) mustBe Some(0.0D)
        extract(JsNumber(1L)) mustBe Some(1.0D)
        extract(JsNumber(-1L)) mustBe Some(-1.0D)

        extract(JsNumber(16777215L)) mustBe Some(16777215.0D)
        extract(JsNumber(16777216L)) mustBe Some(16777216.0D)
        extract(JsNumber(16777218L)) mustBe Some(16777218.0D)

        extract(JsNumber(33554432L)) mustBe Some(33554432.0D)
        extract(JsNumber(33554436L)) mustBe Some(33554436.0D)

        extract(JsNumber(Long.MaxValue)) mustBe Some(9223372036854776000.0D)
        extract(JsNumber(Long.MinValue)) mustBe Some(Long.MinValue.toDouble)
      }

      "given JsBigInteger" in {
        extract(JsNumber(BigInt(0))) mustBe Some(0.0D)
        extract(JsNumber(BigInt(1))) mustBe Some(1.0D)
        extract(JsNumber(BigInt(-1))) mustBe Some(-1.0D)

        extract(JsNumber(BigInt(16777215))) mustBe Some(16777215.0D)
        extract(JsNumber(BigInt(16777216))) mustBe Some(16777216.0D)
        extract(JsNumber(BigInt(33554432))) mustBe Some(33554432.0D)

        extract(JsNumber(BigInt(Long.MinValue))) mustBe Some(Long.MinValue.toDouble)
        extract(JsNumber(BigInt(Long.MaxValue))) mustBe None

        val expected = BigInt(Long.MaxValue) + 1

        extract(JsNumber(expected)) mustBe Some(expected)
      }

      "given JsNumericFloat" in {
        extract(JsNumber(0.0D)) mustBe Some(0.0D)
        extract(JsNumber(-0.0D)) mustBe Some(-0.0D)
        extract(JsNumber(1.0D)) mustBe Some(1.0D)
        extract(JsNumber(-1.0D)) mustBe Some(-1.0D)
        extract(JsNumber(1.5D)) mustBe Some(1.5D)
        extract(JsNumber(-1.5D)) mustBe Some(-1.5D)
        extract(JsNumber(123.5D)) mustBe Some(123.5D)

        extract(JsNumber(Double.MaxValue)) mustBe Some(Double.MaxValue)
        extract(JsNumber(-Double.MaxValue)) mustBe Some(-Double.MaxValue)
        extract(JsNumber(Double.MinPositiveValue)) mustBe Some(Double.MinPositiveValue)
        extract(JsNumber(-Double.MinPositiveValue)) mustBe Some(-Double.MinPositiveValue)

        extract(JsNumber(0.0D)) mustBe Some(0.0D)
        extract(JsNumber(-0.0D)) mustBe Some(-0.0D)
        extract(JsNumber(1.0D)) mustBe Some(1.0D)
        extract(JsNumber(-1.0D)) mustBe Some(-1.0D)
        extract(JsNumber(1.5D)) mustBe Some(1.5D)

        extract(JsNumber(0.1D)) mustBe Some(0.1D)
        extract(JsNumber(1.1D)) mustBe Some(1.1D)
      }

      "given JsLazy integer" in {
        extract(
          new JsLazy(NumberType.Integer, "0", BigDecimal(0))
        ) mustBe Some(0.0D)

        extract(
          new JsLazy(NumberType.Integer, "1", BigDecimal(1))
        ) mustBe Some(1.0D)

        extract(
          new JsLazy(NumberType.Integer, "-1", BigDecimal(-1))
        ) mustBe Some(-1.0D)

        extract(
          new JsLazy(NumberType.Integer, "16777216", BigDecimal(16777216))
        ) mustBe Some(16777216.0D)

        extract(
          new JsLazy(NumberType.Integer, "-16777216", BigDecimal(-16777216))
        ) mustBe Some(-16777216.0D)

        extract(
          new JsLazy(NumberType.Integer, Int.MinValue.toString, BigDecimal(Int.MinValue))
        ) mustBe Some(Int.MinValue.toDouble)
      }

      "given JsLazy float" in {
        extract(
          new JsLazy(NumberType.Float, "0.0", BigDecimal("0.0"))
        ) mustBe Some(0.0D)

        extract(
          new JsLazy(NumberType.Float, "-0.0", BigDecimal("-0.0"))
        ) mustBe Some(-0.0D)

        extract(
          new JsLazy(NumberType.Float, "1.0", BigDecimal("1.0"))
        ) mustBe Some(1.0D)

        extract(
          new JsLazy(NumberType.Float, "-1.0", BigDecimal("-1.0"))
        ) mustBe Some(-1.0D)

        extract(
          new JsLazy(NumberType.Float, "1.5", BigDecimal("1.5"))
        ) mustBe Some(1.5D)

        extract(
          new JsLazy(NumberType.Float, "-1.5", BigDecimal("-1.5"))
        ) mustBe Some(-1.5D)

        extract(
          new JsLazy(NumberType.Float, "16777216.0", BigDecimal("16777216.0"))
        ) mustBe Some(16777216.0D)

        extract(
          new JsLazy(NumberType.Float, "1e0", BigDecimal("1e0"))
        ) mustBe Some(1.0D)

        extract(
          new JsLazy(NumberType.Float, "15e-1", BigDecimal("15e-1"))
        ) mustBe Some(1.5D)

        extract(
          new JsLazy(NumberType.Float, "1e39", BigDecimal("1e39"))
        ) mustBe Some(1E39)

        extract(
          new JsLazy(NumberType.Float, "-1e39", BigDecimal("-1e39"))
        ) mustBe Some(-1E39)
      }

      "given JsBigDecimal" in {
        extract(JsNumber(BigDecimal(0))) mustBe Some(0.0D)
        extract(JsNumber(BigDecimal(1))) mustBe Some(1.0D)
        extract(JsNumber(BigDecimal(-1))) mustBe Some(-1.0D)

        extract(JsNumber(BigDecimal("1.5"))) mustBe Some(1.5D)
        extract(JsNumber(BigDecimal("-1.5"))) mustBe Some(-1.5D)

        extract(JsNumber(BigDecimal("16777216"))) mustBe Some(16777216.0D)
        extract(JsNumber(BigDecimal("16777218"))) mustBe Some(16777218.0D)

        extract(JsNumber(BigDecimal(Long.MinValue))) mustBe Some(BigDecimal(Long.MinValue).toDouble)
        extract(JsNumber(BigDecimal(Long.MaxValue))) mustBe Some(9223372036854775807.0D)
      }
    }
  }
}
