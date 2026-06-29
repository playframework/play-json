/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import org.scalatest.EitherValues
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

final class MacroScala3Spec
    extends AnyWordSpec
    with Matchers
    with EitherValues
    with org.scalatestplus.scalacheck.ScalaCheckPropertyChecks {
  "Case class" should {
    "not be handled" when {
      "no custom ProductOf" in {
        "Json.writes[CustomNoProductOf]" mustNot typeCheck
      }
    }

    "be handled" when {
      "is declared with more than 22 fields" in {
        val format = Json.format[BigFat]

        format
          .writes(BigFat.example)
          .mustEqual(
            Json.obj(
              "e" -> Seq(1, 2, 3),
              "n" -> "n",
              "t" -> Seq(8),
              "a" -> 1,
              "m" -> 12,
              "i" -> "i",
              "v" -> "v",
              "p" -> 13,
              "r" -> 15,
              "w" -> Seq(9, 10, 11),
              "k" -> 10,
              "s" -> "s",
              "x" -> 12,
              "j" -> Seq(4, 5),
              "y" -> Seq(13, 14),
              "u" -> 16,
              "f" -> 6,
              "q" -> 14,
              "b" -> 2,
              "g" -> 7,
              "l" -> 11,
              "c" -> 3,
              "h" -> 8,
              "o" -> Seq(6, 7),
              "z" -> 15,
              "d" -> "d"
            )
          )
      }
    }
  }

  "Scala 3 enum" should {
    "be handled" when {
      "declared with no-arg cases" in {
        given Format[Color.Red.type]   = Json.format[Color.Red.type]
        given Format[Color.Green.type] = Json.format[Color.Green.type]
        given Format[Color.Blue.type]  = Json.format[Color.Blue.type]
        val format                     = Json.format[Color]

        val redJson = Json.obj("_type" -> "play.api.libs.json.Color.Red")
        format.writes(Color.Red).mustEqual(redJson)
        format.reads(redJson).asEither.value.mustEqual(Color.Red)

        val greenJson = Json.obj("_type" -> "play.api.libs.json.Color.Green")
        format.writes(Color.Green).mustEqual(greenJson)
        format.reads(greenJson).asEither.value.mustEqual(Color.Green)

        val blueJson = Json.obj("_type" -> "play.api.libs.json.Color.Blue")
        format.writes(Color.Blue).mustEqual(blueJson)
        format.reads(blueJson).asEither.value.mustEqual(Color.Blue)
      }
    }

    "declared with single-arg case" in {
      given Format[IntOption.Some] = Json.format[IntOption.Some]
      given Format[IntOption.None.type] = Json.format[IntOption.None.type]
      given format: Format[IntOption] = Json.format[IntOption]

      val someValue = IntOption.Some(1)
      val someJson = Json.obj("_type" -> "play.api.libs.json.IntOption.Some", "value" -> 1)
      format.writes(someValue).mustEqual(someJson)
      format.reads(someJson).asEither.value.mustEqual(someValue)

      val noneJson = Json.obj("_type" -> "play.api.libs.json.IntOption.None")
      format.writes(IntOption.None).mustEqual(noneJson)
      format.reads(noneJson).asEither.value.mustEqual(IntOption.None)
    }
  }
}

final class CustomNoProductOf(val name: String, val age: Int)

object CustomNoProductOf {

  given Conversion[CustomNoProductOf, Tuple2[String, Int]] =
    (v: CustomNoProductOf) => v.name -> v.age
}

enum Color {
  case Red, Green, Blue
}

enum IntOption {
  case Some(value: Int)
  case None
}
