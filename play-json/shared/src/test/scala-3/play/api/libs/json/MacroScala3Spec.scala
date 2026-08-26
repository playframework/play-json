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

  "Enum" should {
    "be supported with default string representation" when {
      def readsSpecs(r: Reads[Color]) = {
        r.reads(JsString("Red")).mustEqual(JsSuccess(Color.Red))
        r.reads(JsString("Green")).mustEqual(JsSuccess(Color.Green))
        r.reads(JsString("Blue")).mustEqual(JsSuccess(Color.Blue))

        r.reads(JsString("red")).mustEqual(JsError("error.expected.enum"))
        r.reads(JsString("GREEN")).mustEqual(JsError("error.expected.enum"))
        r.reads(JsString("BluE")).mustEqual(JsError("error.expected.enum"))
      }

      "read" in {
        readsSpecs(Json.enumReads[Color])
        readsSpecs(Json.enumReads(insensitive = false))
      }

      def writesSpecs(w: Writes[Color]) = {
        w.writes(Color.Red).mustEqual(JsString("Red"))
        w.writes(Color.Green).mustEqual(JsString("Green"))
        w.writes(Color.Blue).mustEqual(JsString("Blue"))
      }

      "write" in {
        writesSpecs(Json.enumWrites[Color])
      }

      "format" in {
        val f: Format[Color] = Json.enumFormat

        readsSpecs(f)
        readsSpecs(Json.enumFormat(insensitive = false))

        writesSpecs(f)
      }
    }

    "be supported with lower case representation" when {
      def readsSpecs(r: Reads[Color]) = {
        r.reads(JsString("red")).mustEqual(JsSuccess(Color.Red))
        r.reads(JsString("green")).mustEqual(JsSuccess(Color.Green))
        r.reads(JsString("blue")).mustEqual(JsSuccess(Color.Blue))

        r.reads(JsString("Red")).mustEqual(JsError("error.expected.enum"))
        r.reads(JsString("GREEN")).mustEqual(JsError("error.expected.enum"))
        r.reads(JsString("BluE")).mustEqual(JsError("error.expected.enum"))
      }

      "read" in {
        readsSpecs(Json.enumReadsLowercaseOnly[Color])
      }

      def writesSpecs(w: Writes[Color]) = {
        w.writes(Color.Red).mustEqual(JsString("red"))
        w.writes(Color.Green).mustEqual(JsString("green"))
        w.writes(Color.Blue).mustEqual(JsString("blue"))
      }

      "write" in {
        writesSpecs(Json.enumWritesLowercase[Color])
      }

      "format" in {
        val f: Format[Color] = Json.enumFormatLowercaseOnly

        readsSpecs(f)
        writesSpecs(f)
      }
    }

    "be supported with upper case representation" when {
      def readsSpecs(r: Reads[Color]) = {
        r.reads(JsString("RED")).mustEqual(JsSuccess(Color.Red))
        r.reads(JsString("GREEN")).mustEqual(JsSuccess(Color.Green))
        r.reads(JsString("BLUE")).mustEqual(JsSuccess(Color.Blue))

        r.reads(JsString("Red")).mustEqual(JsError("error.expected.enum"))
        r.reads(JsString("green")).mustEqual(JsError("error.expected.enum"))
        r.reads(JsString("BluE")).mustEqual(JsError("error.expected.enum"))
      }

      "read" in {
        readsSpecs(Json.enumReadsUppercaseOnly[Color])
      }

      def writesSpecs(w: Writes[Color]) = {
        w.writes(Color.Red).mustEqual(JsString("RED"))
        w.writes(Color.Green).mustEqual(JsString("GREEN"))
        w.writes(Color.Blue).mustEqual(JsString("BLUE"))
      }

      "write" in {
        writesSpecs(Json.enumWritesUppercase[Color])
      }

      "format" in {
        val f: Format[Color] = Json.enumFormatUppercaseOnly

        readsSpecs(f)
        writesSpecs(f)
      }
    }

    "ignore case" when {
      def readsSpecs(r: Reads[Color]) = {
        r.reads(JsString("Red")).mustEqual(JsSuccess(Color.Red))
        r.reads(JsString("red")).mustEqual(JsSuccess(Color.Red))
        r.reads(JsString("RED")).mustEqual(JsSuccess(Color.Red))
        r.reads(JsString("ReD")).mustEqual(JsSuccess(Color.Red))

        r.reads(JsString("Green")).mustEqual(JsSuccess(Color.Green))
        r.reads(JsString("green")).mustEqual(JsSuccess(Color.Green))
        r.reads(JsString("GREEN")).mustEqual(JsSuccess(Color.Green))
        r.reads(JsString("GrEeN")).mustEqual(JsSuccess(Color.Green))

        r.reads(JsString("Blue")).mustEqual(JsSuccess(Color.Blue))
        r.reads(JsString("blue")).mustEqual(JsSuccess(Color.Blue))
        r.reads(JsString("BLUE")).mustEqual(JsSuccess(Color.Blue))
        r.reads(JsString("BlUE")).mustEqual(JsSuccess(Color.Blue))
      }

      "read" in {
        readsSpecs(Json.enumReads(insensitive = true))
      }

      "format" in {
        readsSpecs(Json.enumFormat(insensitive = true))
      }
    }
  }
}

final class CustomNoProductOf(val name: String, val age: Int)

object CustomNoProductOf {

  given Conversion[CustomNoProductOf, Tuple2[String, Int]] =
    (v: CustomNoProductOf) => v.name -> v.age
}

enum Color:
  case Red, Green, Blue
