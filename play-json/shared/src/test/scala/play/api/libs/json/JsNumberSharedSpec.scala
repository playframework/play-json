/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import org.scalacheck.{ Arbitrary, Gen }

import org.scalatest.matchers.must.Matchers, Matchers._
import org.scalatest.wordspec.AnyWordSpec

import org.scalatest.Inspectors.{ forAll => foreach }

final class JsNumberSharedSpec
    extends AnyWordSpec
    with Matchers
    with org.scalatestplus.scalacheck.ScalaCheckPropertyChecks {

  "Implementation" should {
    "be expected one" in {
      JsNumber(BigDecimal(1)).isInstanceOf[JsNumber.JsBigDecimal] mustBe true

      JsNumber(2).isInstanceOf[JsNumber.JsNumericInt[Int]] mustBe true

      JsNumber(3.toShort).isInstanceOf[JsNumber.JsNumericInt[Short]] mustBe true

      JsNumber(4L).isInstanceOf[JsNumber.JsLong] mustBe true

      JsNumber(5.6F).isInstanceOf[JsNumber.JsNumericDouble] mustBe true

      JsNumber(7.89D).isInstanceOf[JsNumber.JsNumericDouble] mustBe true

      (new JsNumber.JsLazy(numberType = JsNumber.NumberType.Float, text = "12.3", BigDecimal("12.3")))
        .isInstanceOf[JsNumber.JsLazy] mustBe true
    }
  }

  "JsNumber equality" should {
    import JsNumber._

    def float(v: Float) =
      new JsNumericDouble(v.toDouble, BigDecimal(v), v)

    def double(d: Double) =
      new JsNumericDouble(d, BigDecimal(d), d.toFloat)

    def long(v: Long) =
      new JsLong(v)

    def int(v: Int) =
      new JsNumericInt(v, implicitly[Numeric[Int]])

    def bigInt(v: BigInt) =
      new JsBigInteger(v)

    def bigDec(v: BigDecimal) =
      new JsBigDecimal(v)

    def lazyNum(t: NumberType, text: String, value: BigDecimal) =
      new JsLazy(t, text, value)

    "be symmetric and hashCode-consistent for equal methods" when {
      def eq(left: JsNumber, right: JsNumber) = {
        left mustEqual right

        right mustEqual left

        left.hashCode mustEqual right.hashCode
      }

      foreach(
        Seq[(JsNumber, JsNumber)](
          // Same type
          long(0L)            -> long(0L),
          long(Long.MinValue) -> long(Long.MinValue),
          long(Long.MaxValue) -> long(Long.MaxValue),

          int(0)            -> int(0),
          int(Int.MinValue) -> int(Int.MinValue),
          int(Int.MaxValue) -> int(Int.MaxValue),

          bigInt(BigInt(0))                 -> bigInt(BigInt(0)),
          bigInt(BigInt(Int.MinValue))      -> bigInt(BigInt(Int.MinValue)),
          bigInt(BigInt(Int.MaxValue))      -> bigInt(BigInt(Int.MaxValue)),
          bigInt(BigInt(Long.MinValue))     -> bigInt(BigInt(Long.MinValue)),
          bigInt(BigInt(Long.MaxValue))     -> bigInt(BigInt(Long.MaxValue)),
          bigInt(BigInt(Long.MinValue) - 1) ->
            bigInt(BigInt(Long.MinValue) - 1),
          bigInt(BigInt(Long.MaxValue) + 1) ->
            bigInt(BigInt(Long.MaxValue) + 1),

          float(0.0F) -> float(0.0F),
          float(1.5F) -> float(1.5F),

          double(0.1D) -> double(0.1D),
          double(1.6D) -> double(1.6D),

          bigDec(BigDecimal(0))     -> bigDec(BigDecimal(0)),
          bigDec(BigDecimal("1.5")) -> bigDec(BigDecimal("1.5")),

          // Long <-> Int
          long(0L)                  -> int(0),
          int(0)                    -> long(0L),
          long(Int.MinValue.toLong) -> int(Int.MinValue),
          int(Int.MinValue)         -> long(Int.MinValue.toLong),
          long(Int.MaxValue.toLong) -> int(Int.MaxValue),
          int(Int.MaxValue)         -> long(Int.MaxValue.toLong),

          // BigInteger <-> Int
          bigInt(BigInt(0))            -> int(0),
          int(0)                       -> bigInt(BigInt(0)),
          bigInt(BigInt(Int.MinValue)) -> int(Int.MinValue),
          int(Int.MinValue)            -> bigInt(BigInt(Int.MinValue)),
          bigInt(BigInt(Int.MaxValue)) -> int(Int.MaxValue),
          int(Int.MaxValue)            -> bigInt(BigInt(Int.MaxValue)),

          // BigInteger <-> Long
          bigInt(BigInt(0))             -> long(0L),
          long(0L)                      -> bigInt(BigInt(0)),
          bigInt(BigInt(Long.MinValue)) -> long(Long.MinValue),
          long(Long.MinValue)           -> bigInt(BigInt(Long.MinValue)),
          bigInt(BigInt(Long.MaxValue)) -> long(Long.MaxValue),
          long(Long.MaxValue)           -> bigInt(BigInt(Long.MaxValue)),

          // Long <-> Float
          long(0L)    -> float(0.0F),
          float(0.0F) -> long(0L),
          long(1L)    -> float(1.0F),
          float(1.0F) -> long(1L),

          // Long <-> Double
          long(2L)     -> double(2.0D),
          double(2.0D) -> long(2L),
          long(3L)     -> double(3.0D),
          double(3.0D) -> long(3L),

          // Int <-> Float
          int(0)      -> float(0.0F),
          float(0.0F) -> int(0),
          int(1)      -> float(1.0F),
          float(1.0F) -> int(1),

          // Int <-> Double
          int(2)       -> double(2.0D),
          double(2.0D) -> int(2),
          int(3)       -> double(3.0D),
          double(3.0D) -> int(3),

          // Int <-> Lazy integer
          int(Int.MinValue) ->
            lazyNum(
              NumberType.Integer,
              Int.MinValue.toString,
              BigDecimal(Int.MinValue)
            ),
          lazyNum(
            NumberType.Integer,
            Int.MinValue.toString,
            BigDecimal(Int.MinValue)
          ) -> int(Int.MinValue),

          int(Int.MaxValue) ->
            lazyNum(
              NumberType.Integer,
              Int.MaxValue.toString,
              BigDecimal(Int.MaxValue)
            ),
          lazyNum(
            NumberType.Integer,
            Int.MaxValue.toString,
            BigDecimal(Int.MaxValue)
          ) -> int(Int.MaxValue),

          // Long <-> Lazy integer
          long(Long.MinValue) ->
            lazyNum(
              NumberType.Integer,
              Long.MinValue.toString,
              BigDecimal(Long.MinValue)
            ),
          lazyNum(
            NumberType.Integer,
            Long.MinValue.toString,
            BigDecimal(Long.MinValue)
          ) -> long(Long.MinValue),

          long(Long.MaxValue) ->
            lazyNum(
              NumberType.Integer,
              Long.MaxValue.toString,
              BigDecimal(Long.MaxValue)
            ),
          lazyNum(
            NumberType.Integer,
            Long.MaxValue.toString,
            BigDecimal(Long.MaxValue)
          ) -> long(Long.MaxValue),

          // BigInteger <-> Lazy integer
          bigInt(BigInt(Int.MinValue)) ->
            lazyNum(
              NumberType.Integer,
              Int.MinValue.toString,
              BigDecimal(Int.MinValue)
            ),
          lazyNum(
            NumberType.Integer,
            Int.MinValue.toString,
            BigDecimal(Int.MinValue)
          ) -> bigInt(BigInt(Int.MinValue)),

          bigInt(BigInt(Int.MaxValue)) ->
            lazyNum(
              NumberType.Integer,
              Int.MaxValue.toString,
              BigDecimal(Int.MaxValue)
            ),
          lazyNum(
            NumberType.Integer,
            Int.MaxValue.toString,
            BigDecimal(Int.MaxValue)
          ) -> bigInt(BigInt(Int.MaxValue)),

          bigInt(BigInt(Long.MinValue)) ->
            lazyNum(
              NumberType.Integer,
              Long.MinValue.toString,
              BigDecimal(Long.MinValue)
            ),
          lazyNum(
            NumberType.Integer,
            Long.MinValue.toString,
            BigDecimal(Long.MinValue)
          ) -> bigInt(BigInt(Long.MinValue)),

          bigInt(BigInt(Long.MaxValue)) ->
            lazyNum(
              NumberType.Integer,
              Long.MaxValue.toString,
              BigDecimal(Long.MaxValue)
            ),
          lazyNum(
            NumberType.Integer,
            Long.MaxValue.toString,
            BigDecimal(Long.MaxValue)
          ) -> bigInt(BigInt(Long.MaxValue)),

          bigInt(BigInt(Long.MinValue) - 1) ->
            lazyNum(
              NumberType.Integer,
              "-9223372036854775809",
              BigDecimal("-9223372036854775809")
            ),
          lazyNum(
            NumberType.Integer,
            "-9223372036854775809",
            BigDecimal("-9223372036854775809")
          ) -> bigInt(BigInt(Long.MinValue) - 1),

          bigInt(BigInt(Long.MaxValue) + 1) ->
            lazyNum(
              NumberType.Integer,
              "9223372036854775808",
              BigDecimal("9223372036854775808")
            ),
          lazyNum(
            NumberType.Integer,
            "9223372036854775808",
            BigDecimal("9223372036854775808")
          ) -> bigInt(BigInt(Long.MaxValue) + 1),

          // Float <-> Lazy float
          float(1.5F) ->
            lazyNum(NumberType.Float, "1.5", BigDecimal("1.5")),
          lazyNum(NumberType.Float, "1.5", BigDecimal("1.5")) ->
            float(1.5F),

          // Double <-> Lazy float
          double(2.5D) ->
            lazyNum(NumberType.Float, "2.5", BigDecimal("2.5")),
          lazyNum(NumberType.Float, "2.5", BigDecimal("2.5")) ->
            double(2.5D),

          // BigDecimal <-> Int
          bigDec(BigDecimal(Int.MinValue)) -> int(Int.MinValue),
          int(Int.MinValue)                -> bigDec(BigDecimal(Int.MinValue)),
          bigDec(BigDecimal(Int.MaxValue)) -> int(Int.MaxValue),
          int(Int.MaxValue)                -> bigDec(BigDecimal(Int.MaxValue)),

          // BigDecimal <-> Long
          bigDec(BigDecimal(Long.MinValue)) -> long(Long.MinValue),
          long(Long.MinValue)               -> bigDec(BigDecimal(Long.MinValue)),
          bigDec(BigDecimal(Long.MaxValue)) -> long(Long.MaxValue),
          long(Long.MaxValue)               -> bigDec(BigDecimal(Long.MaxValue)),

          // BigDecimal <-> BigInteger
          bigDec(BigDecimal(0))            -> bigInt(BigInt(0)),
          bigInt(BigInt(0))                -> bigDec(BigDecimal(0)),
          bigDec(BigDecimal(Int.MinValue)) ->
            bigInt(BigInt(Int.MinValue)),
          bigInt(BigInt(Int.MinValue)) ->
            bigDec(BigDecimal(Int.MinValue)),
          bigDec(BigDecimal(Int.MaxValue)) ->
            bigInt(BigInt(Int.MaxValue)),
          bigInt(BigInt(Int.MaxValue)) ->
            bigDec(BigDecimal(Int.MaxValue)),
          bigDec(BigDecimal(Long.MinValue)) ->
            bigInt(BigInt(Long.MinValue)),
          bigInt(BigInt(Long.MinValue)) ->
            bigDec(BigDecimal(Long.MinValue)),
          bigDec(BigDecimal(Long.MaxValue)) ->
            bigInt(BigInt(Long.MaxValue)),
          bigInt(BigInt(Long.MaxValue)) ->
            bigDec(BigDecimal(Long.MaxValue)),
          bigDec(BigDecimal(Long.MinValue) - 1) ->
            bigInt(BigInt(Long.MinValue) - 1),
          bigInt(BigInt(Long.MinValue) - 1) ->
            bigDec(BigDecimal(Long.MinValue) - 1),
          bigDec(BigDecimal(Long.MaxValue) + 1) ->
            bigInt(BigInt(Long.MaxValue) + 1),
          bigInt(BigInt(Long.MaxValue) + 1) ->
            bigDec(BigDecimal(Long.MaxValue) + 1),

          // BigDecimal <-> Float
          bigDec(BigDecimal("1.5")) -> float(1.5F),
          float(1.5F)               -> bigDec(BigDecimal("1.5")),

          // BigDecimal <-> Double
          bigDec(BigDecimal("3.5")) -> double(3.5F),
          double(3.5F)              -> bigDec(BigDecimal("3.5")),

          // BigDecimal <-> Lazy
          bigDec(BigDecimal(1)) ->
            lazyNum(NumberType.Integer, "1", BigDecimal(1)),
          lazyNum(NumberType.Integer, "1", BigDecimal(1)) ->
            bigDec(BigDecimal(1)),

          bigDec(BigDecimal("1.5")) ->
            lazyNum(NumberType.Float, "1.5", BigDecimal("1.5")),
          lazyNum(NumberType.Float, "1.5", BigDecimal("1.5")) ->
            bigDec(BigDecimal("1.5"))
        )
      ) { case (left, right) =>
        s"between ${left.getClass.getSimpleName}/$left & ${right.getClass.getSimpleName}/$right" in {
          eq(left, right)
        }
      }

      "be consistent" in {
        def genEqualJsNumberPair: Gen[(JsNumber, JsNumber)] =
          JsNumberSharedSpec.genJsNumber.flatMap { left =>
            Gen.oneOf(
              Gen.const(left -> left),
              JsNumberSharedSpec.genJsNumber
                .filter(_.equals(left))
                .map(right => left -> right)
            )
          }

        // -- scalacheck.mintestsok <n>
        forAll(genEqualJsNumberPair) { (data: Tuple2[JsNumber, JsNumber]) =>
          val (left, right) = data

          /* DEBUG:
        if (left != right) {
          println(s"${left.getClass.getSimpleName}/$left != ${right.getClass.getSimpleName}/$right")
        } */

          left.mustEqual(left)

          right.mustEqual(right)

          left.mustEqual(right)

          right.mustEqual(left)

          left.hashCode.mustEqual(left.hashCode)
        }
      }
    }

    "reject non-equal values symmetrically" when {
      def notEq(left: JsNumber, right: JsNumber) = {
        left.must(not).equal(right)

        right.must(not).equal(left)
      }

      foreach(
        Seq[(JsNumber, JsNumber)](
          // Same type
          long(0L)            -> long(1L),
          long(Long.MinValue) -> long(Long.MaxValue),

          int(0)            -> int(1),
          int(Int.MinValue) -> int(Int.MaxValue),

          bigInt(BigInt(0))                 -> bigInt(BigInt(1)),
          bigInt(BigInt(Long.MinValue))     -> bigInt(BigInt(Long.MaxValue)),
          bigInt(BigInt(Long.MinValue) - 1) ->
            bigInt(BigInt(Long.MinValue)),
          bigInt(BigInt(Long.MaxValue) + 1) ->
            bigInt(BigInt(Long.MaxValue)),

          float(0.0F) -> float(1.0F),
          float(1.5F) -> float(2.5F),

          bigDec(BigDecimal(0))     -> bigDec(BigDecimal(1)),
          bigDec(BigDecimal("1.5")) -> bigDec(BigDecimal("2.5")),

          // Long <-> Int
          long(1L) -> int(2),
          int(2)   -> long(1L),

          // BigInteger <-> Int
          bigInt(BigInt(Int.MaxValue) + 1) -> int(Int.MaxValue),
          int(Int.MaxValue)                -> bigInt(BigInt(Int.MaxValue) + 1),
          bigInt(BigInt(Int.MinValue) - 1) -> int(Int.MinValue),
          int(Int.MinValue)                -> bigInt(BigInt(Int.MinValue) - 1),

          // BigInteger <-> Long
          JsNumber(9223372036854775807L)           -> JsNumber(BigInt("-9223372036854775809")),
          JsNumber(BigInt("-9223372036854775809")) -> JsNumber(9223372036854775807L),
          bigInt(BigInt(Long.MaxValue) + 1)        -> long(Long.MaxValue),
          long(Long.MaxValue)                      -> bigInt(BigInt(Long.MaxValue) + 1),
          bigInt(BigInt(Long.MinValue) - 1)        -> long(Long.MinValue),
          long(Long.MinValue)                      -> bigInt(BigInt(Long.MinValue) - 1),

          // Long <-> Float
          long(1L)    -> float(1.5F),
          float(1.5F) -> long(1L),

          // Int <-> Float
          int(1)      -> float(1.5F),
          float(1.5F) -> int(1),

          // Int <-> Lazy integer: just outside Int range
          int(Int.MaxValue) ->
            lazyNum(
              NumberType.Integer,
              "2147483648",
              BigDecimal("2147483648")
            ),
          lazyNum(
            NumberType.Integer,
            "2147483648",
            BigDecimal("2147483648")
          ) -> int(Int.MaxValue),

          int(Int.MinValue) ->
            lazyNum(
              NumberType.Integer,
              "-2147483649",
              BigDecimal("-2147483649")
            ),
          lazyNum(
            NumberType.Integer,
            "-2147483649",
            BigDecimal("-2147483649")
          ) -> int(Int.MinValue),

          // Long <-> Lazy integer: just outside Long range
          long(Long.MaxValue) ->
            lazyNum(
              NumberType.Integer,
              "9223372036854775808",
              BigDecimal("9223372036854775808")
            ),
          lazyNum(
            NumberType.Integer,
            "9223372036854775808",
            BigDecimal("9223372036854775808")
          ) -> long(Long.MaxValue),

          long(Long.MinValue) ->
            lazyNum(
              NumberType.Integer,
              "-9223372036854775809",
              BigDecimal("-9223372036854775809")
            ),
          lazyNum(
            NumberType.Integer,
            "-9223372036854775809",
            BigDecimal("-9223372036854775809")
          ) -> long(Long.MinValue),

          // BigInteger <-> Lazy integer
          bigInt(BigInt(Long.MaxValue) + 1) ->
            lazyNum(
              NumberType.Integer,
              "9223372036854775809",
              BigDecimal("9223372036854775809")
            ),
          lazyNum(
            NumberType.Integer,
            "9223372036854775809",
            BigDecimal("9223372036854775809")
          ) -> bigInt(BigInt(Long.MaxValue) + 1),

          bigInt(BigInt(Long.MinValue) - 1) ->
            lazyNum(
              NumberType.Integer,
              "-9223372036854775810",
              BigDecimal("-9223372036854775810")
            ),
          lazyNum(
            NumberType.Integer,
            "-9223372036854775810",
            BigDecimal("-9223372036854775810")
          ) -> bigInt(BigInt(Long.MinValue) - 1),

          // BigInteger <-> BigDecimal
          bigInt(BigInt(123))           -> bigDec(BigDecimal(124)),
          bigDec(BigDecimal(124))       -> bigInt(BigInt(123)),
          bigInt(BigInt(Long.MaxValue)) ->
            bigDec(BigDecimal(Long.MaxValue) + 1),
          bigDec(BigDecimal(Long.MaxValue) + 1) ->
            bigInt(BigInt(Long.MaxValue)),

          // Float <-> Lazy float
          float(1.5F) ->
            lazyNum(NumberType.Float, "2.5", BigDecimal("2.5")),
          lazyNum(NumberType.Float, "2.5", BigDecimal("2.5")) ->
            float(1.5F),

          // Lazy integer <-> Lazy float
          lazyNum(NumberType.Integer, "1", BigDecimal(1)) ->
            lazyNum(NumberType.Float, "1.0", BigDecimal("1.0")),
          lazyNum(NumberType.Float, "1.0", BigDecimal("1.0")) ->
            lazyNum(NumberType.Integer, "1", BigDecimal(1)),

          // BigDecimal just outside Int range
          bigDec(BigDecimal(Int.MaxValue) + 1) -> int(Int.MaxValue),
          int(Int.MaxValue)                    -> bigDec(BigDecimal(Int.MaxValue) + 1),

          bigDec(BigDecimal(Int.MinValue) - 1) -> int(Int.MinValue),
          int(Int.MinValue)                    -> bigDec(BigDecimal(Int.MinValue) - 1),

          // BigDecimal just outside Long range
          bigDec(BigDecimal(Long.MaxValue) + 1) -> long(Long.MaxValue),
          long(Long.MaxValue)                   -> bigDec(BigDecimal(Long.MaxValue) + 1),

          bigDec(BigDecimal(Long.MinValue) - 1) -> long(Long.MinValue),
          long(Long.MinValue)                   -> bigDec(BigDecimal(Long.MinValue) - 1),

          // BigDecimal <-> Float
          bigDec(BigDecimal("1.5")) -> float(2.5F),
          float(2.5F)               -> bigDec(BigDecimal("1.5"))
        )
      ) { case (left, right) =>
        s"between ${left.getClass.getSimpleName}/$left & ${right.getClass.getSimpleName}/$right" in {
          notEq(left, right)
        }
      }

      "be consistent" in {
        def genNotEqualJsNumberPair: Gen[(JsNumber, JsNumber)] =
          JsNumberSharedSpec.genJsNumber.flatMap { left =>
            JsNumberSharedSpec.genJsNumber
              .filter(!_.equals(left))
              .map(right => left -> right)
          }

        // -- scalacheck.mintestsok <n>
        forAll(genNotEqualJsNumberPair) { (data: Tuple2[JsNumber, JsNumber]) =>
          val (left, right) = data

          /* DEBUG:
        if (left == right) {
          println(s"${left.getClass.getSimpleName}/$left == ${right.getClass.getSimpleName}/$right")
        } */

          notEq(left, right)
        }
      }
    }
  }

  "Text representation" should {
    "be valid" in forAll(JsNumberSharedSpec.genJsNumber) { n =>
      try {
        BigDecimal(n.text)

        succeed
      } catch {
        case _: Exception =>
          fail(s"Invalid text representation: ${n.text}")
      }
    }
  }
}

object JsNumberSharedSpec {
  val genJsNumericInt: Gen[JsNumber.JsNumericInt[?]] =
    Gen.oneOf(
      Arbitrary.arbByte.arbitrary.map { b =>
        new JsNumber.JsNumericInt(b, implicitly[Numeric[Byte]])
      },
      Arbitrary.arbShort.arbitrary.map { s =>
        new JsNumber.JsNumericInt(s, implicitly[Numeric[Short]])
      },
      Arbitrary.arbInt.arbitrary.map { i =>
        new JsNumber.JsNumericInt(i, implicitly[Numeric[Int]])
      }
    )

  val genJsLong: Gen[JsNumber.JsLong] =
    Arbitrary.arbLong.arbitrary.map(l => new JsNumber.JsLong(l))

  val genJsBigInteger: Gen[JsNumber.JsBigInteger] =
    Arbitrary.arbBigInt.arbitrary.map(i => new JsNumber.JsBigInteger(i))

  val genJsNumericDouble: Gen[JsNumber.JsNumericDouble] =
    Gen.oneOf(
      Arbitrary.arbFloat.arbitrary.map { f =>
        new JsNumber.JsNumericDouble(f.toDouble, BigDecimal(f), f)
      },
      Arbitrary.arbDouble.arbitrary.map { d =>
        new JsNumber.JsNumericDouble(d, BigDecimal(d), d.toFloat)
      }
    )

  val genJsBigDecimal: Gen[JsNumber.JsBigDecimal] =
    Arbitrary.arbBigDecimal.arbitrary.map(b => new JsNumber.JsBigDecimal(b))

  val genJsLazy: Gen[JsNumber.JsLazy] = Gen.oneOf(
    genJsBigInteger.map { n =>
      new JsNumber.JsLazy(
        numberType = JsNumber.NumberType.Integer, // Integer
        text = n.text,
        bigDecimal = n.value
      )
    },
    genJsBigDecimal.map { n =>
      new JsNumber.JsLazy(
        numberType = JsNumber.NumberType.Float, // Float
        text = n.text,
        bigDecimal = n.value
      )
    }
  )

  val genJsNumber: Gen[JsNumber] =
    Gen.oneOf[JsNumber](genJsNumericInt, genJsLong, genJsBigInteger, genJsNumericDouble, genJsBigDecimal, genJsLazy)

}
