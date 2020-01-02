/*
 * Copyright (C) Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import play.api.libs.json.Json._

import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._

import org.scalatest._

class JsonTransSpec extends WordSpec with MustMatchers {
  "JSON transformers " should {
    val js = Json.obj(
      "field1" -> "alpha",
      "field2" -> 123L,
      "field3" -> Json.obj(
        "field31" -> "beta",
        "field32" -> 345
      ),
      "field4" -> Json.arr("alpha", 2, true, Json.obj("field41" -> "toto", "field42" -> "tata"))
    )

    "pick a value at a path" in {
      js.transform(
          (__ \ Symbol("field3")).json.pick
        )
        .get
        .mustEqual(
          Json.obj(
            "field31" -> "beta",
            "field32" -> 345
          )
        )
    }

    "pick a branch" in {
      js.transform(
          (__ \ Symbol("field3")).json.pickBranch
        )
        .get
        .mustEqual(
          Json.obj(
            "field3" -> Json.obj("field31" -> "beta", "field32" -> 345)
          )
        )
    }

    "copy input JSON and update a branch (merge the updated branch with input JSON)" in {
      js.transform(
          (__ \ Symbol("field3")).json.update(
            __.read[JsObject].map { o =>
              o ++ Json.obj("field33" -> false)
            }
          )
        )
        .get
        .mustEqual(
          Json.obj(
            "field1" -> "alpha",
            "field2" -> 123L,
            "field3" -> Json.obj(
              "field31" -> "beta",
              "field32" -> 345,
              "field33" -> false
            ),
            "field4" -> Json.arr("alpha", 2, true, Json.obj("field41" -> "toto", "field42" -> "tata"))
          )
        )
    }

    "pick a branch and update its content" in {
      js.transform(
          (__ \ Symbol("field3")).json.pickBranch(
            (__ \ Symbol("field32")).json.update(
              Reads.of[JsNumber].map { case JsNumber(nb) => JsNumber(nb + 12) }
            ) andThen
              (__ \ Symbol("field31")).json.update(
                Reads.of[JsString].map { case JsString(s) => JsString(s + "toto") }
              )
          )
        )
        .get
        .mustEqual(
          Json.obj(
            "field3" -> Json.obj("field31" -> "betatoto", "field32" -> 357)
          )
        )
    }

    "put a value in a new branch (don't keep passed json)" in {
      js.transform(
          (__ \ Symbol("field3")).json.put(JsNumber(234))
        )
        .get
        .mustEqual(
          Json.obj(
            "field3" -> 234
          )
        )
    }

    "create a new path by copying a branch" in {
      js.transform(
          (__ \ Symbol("field5")).json.copyFrom((__ \ Symbol("field3")).json.pick)
        )
        .get
        .mustEqual(
          Json.obj(
            "field5" -> Json.obj(
              "field31" -> "beta",
              "field32" -> 345
            )
          )
        )
    }

    "copy full json and prune a branch" in {
      js.transform(
          (__ \ Symbol("field3")).json.prune
        )
        .get
        .mustEqual(
          Json.obj(
            "field1" -> "alpha",
            "field2" -> 123L,
            "field4" -> Json.arr("alpha", 2, true, Json.obj("field41" -> "toto", "field42" -> "tata"))
          )
        )
    }

    "pick a single branch and prune a sub-branch" in {
      js.transform(
          (__ \ Symbol("field3")).json.pickBranch(
            (__ \ Symbol("field32")).json.prune
          )
        )
        .get
        .mustEqual(
          Json.obj(
            "field3" -> Json.obj("field31" -> "beta")
          )
        )
    }

    "copy the full json and update a 2nd-level path and then prune a subbranch" in {
      js.validate(
          (__ \ Symbol("field3") \ Symbol("field32")).json.update(
            Reads.of[JsNumber].map { case JsNumber(nb) => JsNumber(nb + 5) }
          ) andThen (__ \ Symbol("field4")).json.prune
        )
        .get
        .mustEqual(
          Json.obj(
            "field1" -> "alpha",
            "field2" -> 123L,
            "field3" -> Json.obj(
              "field31" -> "beta",
              "field32" -> 350
            )
          )
        )
    }

    "deepMerge when reducing JsObjects" in {
      val json = Json.obj("somekey1" -> 11, "somekey2" -> 22)
      val jsonTransform = (
        (__ \ "key1" \ "sk1").json.copyFrom((__ \ "somekey1").json.pick)
          and
            (__ \ "key1" \ "sk2").json.copyFrom((__ \ "somekey2").json.pick)
      ).reduce

      json
        .validate(jsonTransform)
        .get
        .mustEqual(
          Json.obj("key1" -> Json.obj("sk1" -> 11, "sk2" -> 22))
        )
    }

    "report the correct path in the JsError" when {
      "the field to modify doesn't exist" in {
        js.transform(
            (__ \ Symbol("field42")).json.update(__.read[JsString])
          )
          .asEither
          .swap
          .getOrElse(sys.error("unexpected right"))
          .head
          .mustEqual((__ \ Symbol("field42"), Seq(JsonValidationError("error.path.missing"))))
      }

      "the reader is the wrong type" in {
        js.transform(
            (__ \ Symbol("field2")).json.update(__.read[JsString])
          )
          .asEither
          .swap
          .getOrElse(sys.error("unexpected right"))
          .head
          .mustEqual((__ \ Symbol("field2"), Seq(JsonValidationError("error.expected.jsstring"))))
      }
    }
  }
}
