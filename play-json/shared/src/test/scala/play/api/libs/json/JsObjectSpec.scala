/*
 * Copyright (C) 2009-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import play.api.libs.json.Json._

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import ScalaTestPosition._

class JsObjectSpec extends AnyWordSpec with Matchers {
  "JsObject.deepMerge" should {
    "not fail when the objects are empty" in {
      Json.obj().deepMerge(Json.obj()).mustEqual(Json.obj())
    }

    "merge correctly when the source object is empty" in {
      def populatedObj = Json.obj(
        "field1" -> 123,
        "field2" -> "abc",
        "field3" -> JsNull
      )

      populatedObj.deepMerge(Json.obj()).mustEqual(populatedObj)
    }

    "merge correctly when the incoming object is empty" in {
      val populatedObj = Json.obj(
        "field1" -> 123,
        "field2" -> "abc",
        "field3" -> JsNull
      )

      Json.obj().deepMerge(populatedObj).mustEqual(populatedObj)
    }
  }

  "JsObject.deepMerge should keep existing attributes where there is no collision and" should {
    "overwrite existing attributes on collision when value is not a JsArray or JsObject" in {
      Json
        .obj(
          "field1" -> 123,
          "field2" -> "abc",
          "field3" -> JsNull,
          "field4" -> 456,
          "field5" -> "abc",
          "field6" -> "def"
        )
        .deepMerge(
          Json.obj(
            "field4" -> 789,
            "field5" -> "xyz",
            "field6" -> JsNull
          )
        )
        .mustEqual(
          Json.obj(
            "field1" -> 123,
            "field2" -> "abc",
            "field3" -> JsNull,
            "field4" -> 789,
            "field5" -> "xyz",
            "field6" -> JsNull
          )
        )
    }

    "recursively merge where elements are both of type JsArray or both of type JsObject" in {
      Json
        .obj(
          "field1" -> 123,
          "field2" -> "abc",
          "field3" -> Json.arr(
            "abc",
            "def",
            "ghi"
          ),
          "field4" -> Json.obj(
            "field1a" -> 888,
            "field2b" -> "xxx",
            "field3c" -> JsNull
          )
        )
        .deepMerge(
          Json.obj(
            "field3" -> Json.arr(
              "jkl",
              "mno",
              "pqr"
            ),
            "field4" -> Json.obj(
              "field1a" -> 999,
              "field2b" -> "yyy",
              "field3c" -> "zzz"
            )
          )
        )
        .mustEqual(
          Json.obj(
            "field1" -> 123,
            "field2" -> "abc",
            "field3" -> Json.arr(
              "jkl",
              "mno",
              "pqr"
            ),
            "field4" -> Json.obj(
              "field1a" -> 999,
              "field2b" -> "yyy",
              "field3c" -> "zzz"
            )
          )
        )
    }

    "properly merge a deep structure" in {
      Json
        .obj(
          "field1a" -> Json.obj(
            "field2a" -> Json.obj(
              "field3a" -> Json.obj(
                "field4a" -> Json.obj(
                  "field5a" -> "abc",
                  "field5b" -> Json.arr("111", "222"),
                  "field5d" -> Json.arr(Json.obj("a" -> 1), Json.obj("b" -> 2))
                )
              ),
              "field2b" -> Json.arr("aaa", "bbb"),
              "field2c" -> Json.obj(
                "hello" -> "world"
              )
            ),
            "field2b" -> "xxx",
            "field2c" -> JsNull
          )
        )
        .deepMerge(
          Json.obj(
            "field1a" -> Json.obj(
              "field2a" -> Json.obj(
                "field3a" -> Json.obj(
                  "field4a" -> Json.obj(
                    "field5b" -> Json.arr("333", "444"),
                    "field5c" -> "deep",
                    "field5d" -> Json.arr(Json.obj("c" -> 3), Json.obj("d" -> 4))
                  )
                ),
                "field2b" -> Json.arr("ccc", "ddd"),
                "field2c" -> Json.obj(
                  "hello" -> "new world"
                )
              ),
              "field2b" -> "yyy",
              "field2d" -> "zzz"
            )
          )
        )
        .mustEqual(
          Json.obj(
            "field1a" -> Json.obj(
              "field2a" -> Json.obj(
                "field3a" -> Json.obj(
                  "field4a" -> Json.obj(
                    "field5a" -> "abc",
                    "field5b" -> Json.arr("333", "444"),
                    "field5c" -> "deep",
                    "field5d" -> Json.arr(Json.obj("c" -> 3), Json.obj("d" -> 4))
                  )
                ),
                "field2b" -> Json.arr("ccc", "ddd"),
                "field2c" -> Json.obj(
                  "hello" -> "new world"
                )
              ),
              "field2b" -> "yyy",
              "field2c" -> JsNull,
              "field2d" -> "zzz"
            )
          )
        )
    }
  }

  "JsObject.++" should {
    "preserve order of fields" in {
      val obj1 = Json.obj(
        "field1" -> 123,
        "field2" -> "abc",
        "field3" -> JsNull
      )
      val obj2 = Json.obj(
        "field4" -> 456,
        "field5" -> "def",
        "field6" -> JsNull
      )
      val expected = Json.obj(
        "field1" -> 123,
        "field2" -> "abc",
        "field3" -> JsNull,
        "field4" -> 456,
        "field5" -> "def",
        "field6" -> JsNull
      )
      Json.stringify(obj1 ++ obj2).mustEqual(Json.stringify(expected))
    }
  }

  "JsObject.+" should {
    "preserve order of fields" in {
      val obj1 = Json.obj(
        "field1" -> 123,
        "field2" -> "abc",
        "field3" -> JsNull
      )
      val field4 = "field4" -> JsNumber(456)
      val field5 = "field5" -> JsString("def")
      val field6 = "field6" -> JsNull

      val expected = Json.obj(
        "field1" -> 123,
        "field2" -> "abc",
        "field3" -> JsNull,
        "field4" -> 456,
        "field5" -> "def",
        "field6" -> JsNull
      )

      Json.stringify(obj1 + field4 + field5 + field6).mustEqual(Json.stringify(expected))
    }
  }

  "JsObject.-" should {
    "preserve order of fields" in {
      val originalObj = Json.obj(
        "field1" -> 123,
        "field2" -> "abc",
        "field3" -> JsNull,
        "field4" -> 456,
        "field5" -> "def",
        "field6" -> JsNull
      )
      val expected = Json.obj(
        "field4" -> 456,
        "field5" -> "def",
        "field6" -> JsNull
      )

      Json.stringify(originalObj - "field1" - "field2" - "field3").mustEqual(Json.stringify(expected))
    }
  }

  "JsObject" should {
    // see https://github.com/playframework/play-json/issues/390
    "accept null fields when calling Json.obj" in {
      val originalObj = Json.obj(
        "field1" -> null,
        "field2" -> (null: String),
        "field3" -> JsNull
      )
      val expected = """{"field1":null,"field2":null,"field3":null}"""

      Json.stringify(originalObj).mustEqual(expected)
    }

    "accept null fields when calling Json.arr" in {
      val originalObj = Json.obj(
        "arrayWithNulls" -> Json.arr(
          null,
          (null: String),
          JsNull
        )
      )
      val expected = """{"arrayWithNulls":[null,null,null]}"""

      Json.stringify(originalObj).mustEqual(expected)
    }
  }
}
