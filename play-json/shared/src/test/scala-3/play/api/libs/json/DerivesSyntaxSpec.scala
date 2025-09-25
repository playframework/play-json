/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import org.scalatest.EitherValues
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

final class DerivesSyntaxSpec extends AnyWordSpec with Matchers with EitherValues {
  "Derives syntax" should {
    "derive Format using macros" when {
      "used with standalone case classes" in {
        val input = DerivesFormat.StandaloneCaseClass(1)
        val json  = Json.toJson(input)
        json mustBe Json.obj("intField" -> 1)
        Json.fromJson[DerivesFormat.StandaloneCaseClass](json).asEither.value mustBe input
      }

      "used with trait subtypes that are case objects" in {
        val input = DerivesFormat.SomeCaseObject
        val json  = Json.toJson[DerivesFormat.SomeTrait](input)
        json mustBe Json.obj("_type" -> "play.api.libs.json.DerivesFormat.SomeCaseObject")
        Json.fromJson[DerivesFormat.SomeTrait](json).asEither.value mustBe input
      }

      "used with trait subtypes that are case classes" in {
        val input = DerivesFormat.SomeCaseClass("hello")
        val json  = Json.toJson[DerivesFormat.SomeTrait](input)
        json mustBe Json.obj("_type" -> "play.api.libs.json.DerivesFormat.SomeCaseClass", "stringField" -> "hello")
        Json.fromJson[DerivesFormat.SomeTrait](json).asEither.value mustBe input
      }
    }

    "derive Reads using macros" when {
      "used with standalone case classes" in {
        val input    = Json.obj("intField" -> 16)
        val expected = DerivesReads.StandaloneCaseClass(16)
        Json.fromJson[DerivesReads.StandaloneCaseClass](input).asEither.value mustBe expected
      }

      "used with trait subtypes that are case objects" in {
        val input    = Json.obj("_type" -> "play.api.libs.json.DerivesReads.SomeCaseObject")
        val expected = DerivesReads.SomeCaseObject
        Json.fromJson[DerivesReads.SomeTrait](input).asEither.value mustBe expected
      }

      "used with trait subtypes that are case classes" in {
        val input    = Json.obj("_type" -> "play.api.libs.json.DerivesReads.SomeCaseClass", "stringField" -> "abc")
        val expected = DerivesReads.SomeCaseClass("abc")
        Json.fromJson[DerivesReads.SomeTrait](input).asEither.value mustBe expected
      }
    }

    "derive Writes using macros" when {
      "used with standalone case classes" in {
        val input    = DerivesWrites.StandaloneCaseClass(42)
        val expected = Json.obj("intField" -> 42)
        Json.toJson(input) mustBe expected
      }

      "used with trait subtypes that are case objects" in {
        val input    = DerivesWrites.SomeCaseObject
        val expected = Json.obj("_type" -> "play.api.libs.json.DerivesWrites.SomeCaseObject")
        Json.toJson[DerivesWrites.SomeTrait](input) mustBe expected
      }

      "used with trait subtypes that are case classes" in {
        val input    = DerivesWrites.SomeCaseClass("def")
        val expected = Json.obj("_type" -> "play.api.libs.json.DerivesWrites.SomeCaseClass", "stringField" -> "def")
        Json.toJson[DerivesWrites.SomeTrait](input) mustBe expected
      }
    }
  }
}

object DerivesFormat {
  case class StandaloneCaseClass(intField: Int) derives Format
  sealed trait SomeTrait derives Format
  case class SomeCaseClass(stringField: String) extends SomeTrait derives Format
  case object SomeCaseObject                    extends SomeTrait derives Format
}

object DerivesReads {
  case class StandaloneCaseClass(intField: Int) derives Reads
  sealed trait SomeTrait derives Reads
  case class SomeCaseClass(stringField: String) extends SomeTrait derives Reads
  case object SomeCaseObject                    extends SomeTrait derives Reads
}

object DerivesWrites {
  case class StandaloneCaseClass(intField: Int) derives Writes
  sealed trait SomeTrait derives Writes
  case class SomeCaseClass(stringField: String) extends SomeTrait derives Writes
  case object SomeCaseObject                    extends SomeTrait derives Writes
}
