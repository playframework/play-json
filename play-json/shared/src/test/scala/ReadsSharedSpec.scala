/*
 * Copyright (C) 2009-2016 Lightbend Inc. <https://www.lightbend.com>
 */
package play.api.libs.json

import java.util.Locale

import org.scalatest._

class ReadsSharedSpec extends WordSpec with MustMatchers {
  "Reads flatMap" should {
    "not repath the second result" when {
      val aPath = JsPath \ "a"
      val readsA: Reads[String] = aPath.read[String]
      val value = "string"
      val aJson = aPath.write[String].writes(value)

      "in case of success" in {
        val flatMappedReads = readsA.flatMap(_ => readsA)
        aJson.validate(flatMappedReads) mustEqual JsSuccess(value, aPath)
      }

      "in case of failure" in {
        val readsAFail = aPath.read[Int]
        val flatMappedReads = readsA.flatMap(_ => readsAFail)

        aJson.validate(flatMappedReads).
          mustEqual(JsError(List((aPath, List(
            JsonValidationError("error.expected.jsnumber")
          )))))
      }
    }
  }

  "Map" should {
    "be successfully read with string keys" in {
      Json.fromJson[Map[String, Int]](
        Json.obj("foo" -> 1, "bar" -> 2)) mustEqual (
          JsSuccess(Map("foo" -> 1, "bar" -> 2)))
    }

    "be successfully read with character keys" in {
      Json.fromJson[Map[Char, Int]](Json.obj("a" -> 1, "b" -> 2))(
        Reads.charMapReads) mustEqual JsSuccess(Map('a' -> 1, 'b' -> 2))
    }
  }

  "Functionnal Reads" should {
    import play.api.libs.functional.syntax._

    implicit val reads: Reads[Owner] = (
      (__ \ "login").read[String] and
      (__ \ "avatar").read[String] and
      (__ \ "url").read[String]
    )(Owner)

    "be successful for simple case class Owner" in {
      val jsObj = Json.obj(
        "login" -> "foo",
        "avatar" -> "url://avatar",
        "url" -> "url://id"
      )

      Json.parse(Json.stringify(jsObj)) mustEqual jsObj
    }
  }

  // ---

  case class Owner(
    login: String,
    avatar: String,
    url: String
  )
}
