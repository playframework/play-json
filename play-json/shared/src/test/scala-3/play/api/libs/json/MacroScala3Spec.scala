/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

final class MacroScala3Spec
    extends AnyWordSpec
    with Matchers
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
}

final class CustomNoProductOf(val name: String, val age: Int)

object CustomNoProductOf {

  given Conversion[CustomNoProductOf, Tuple2[String, Int]] =
    (v: CustomNoProductOf) => v.name -> v.age
}
