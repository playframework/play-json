/*
 * Copyright (C) 2009-2016 Lightbend Inc. <https://www.lightbend.com>
 */
package play.api.libs.json

import java.util.Locale

import scala.concurrent.duration.{ Duration, FiniteDuration }

import org.scalatest._
import org.scalatest.prop.TableDrivenPropertyChecks._

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

    "be successful for simple case class Owner" in {
      implicit val reads: Reads[Owner] = (
        (__ \ "login").read[String] and
        (__ \ "avatar").read[String] and
        (__ \ "url").read[String]
      )(Owner)

      val jsObj = Json.obj(
        "login" -> "foo",
        "avatar" -> "url://avatar",
        "url" -> "url://id"
      )

      Json.parse(Json.stringify(jsObj)) mustEqual jsObj
    }

    "be successful for FiniteDuration" in {
      forAll(Table(
        "json" -> "expected",
        JsNumber(BigDecimal(0D)) -> JsSuccess(Duration.Zero),
        JsString("1 second") -> JsSuccess(FiniteDuration(1L, "second")),
        JsString("5 seconds") -> JsSuccess(Duration("5seconds")),
        JsNumber(BigDecimal(1.23D)) -> JsError("error.expected.duration")
      )) { (json, expected) =>
        Json.fromJson[Duration](json) mustEqual expected
      }
    }

    "fail for invalid input as Duration" in {
      forAll(Table[JsValue]("json", JsString("foo"), JsNull)) { json =>
        Json.fromJson[Duration](json) mustEqual (
          JsError("error.expected.duration"))
      }
    }

    "fail for invalid input as FiniteDuration" in {
      forAll(Table[JsValue](
        "json",
        JsString("foo"),
        JsNull,
        JsNumber(BigDecimal(1.23D))
      )) { json =>
        Json.fromJson[FiniteDuration](json) mustEqual (
          JsError("error.expected.finiteDuration"))
      }
    }

    "be successful for number as FiniteDuration" in {
      implicit val r = Reads.finiteDurationNumberReads

      forAll(Table(
        "json" -> "expected",
        JsString("0s") -> JsError("error.expected.finiteDuration"),
        JsNumber(BigDecimal(0D)) -> JsSuccess(Duration.Zero),
        JsNumber(BigDecimal(1.23D)) -> JsError("error.expected.finiteDuration"),
        JsNull -> JsError("error.expected.finiteDuration")
      )) { (json, expected) =>
        Json.fromJson[FiniteDuration](json) mustEqual expected
      }
    }

    "be successful for infinite Duration" in forAll(Table(
      "repr" -> "duration",
      "Inf" -> Duration.Inf,
      "PlusInf" -> Duration.Inf,
      "+Inf" -> Duration.Inf,
      "MinusInf" -> Duration.MinusInf,
      "-Inf" -> Duration.MinusInf,
      "Undefined" -> Duration.Undefined
    )) { (repr, duration) =>
      Json.fromJson[Duration](JsString(repr)) mustEqual JsSuccess(duration)

      Json.fromJson[FiniteDuration](JsString(repr)) mustEqual (
        JsError("error.expected.finiteDuration")
      )
    }
  }

  // ---

  case class Owner(
    login: String,
    avatar: String,
    url: String
  )
}
