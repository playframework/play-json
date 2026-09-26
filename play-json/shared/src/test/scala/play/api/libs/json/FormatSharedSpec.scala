/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

final class FormatSharedSpec extends AnyWordSpec with Matchers {
  "Format" should {
    "be bimap'ed" in {
      val strFormat              = implicitly[Format[String]]
      val intFormat: Format[Int] =
        strFormat.bimap(_.size, List.fill(_: Int)('X').mkString)

      intFormat.reads(JsString("foo")).mustEqual(JsSuccess(3))

      intFormat.writes(5).mustEqual(JsString("XXXXX"))
    }
  }

  "OFormat" should {
    "be bimap'ed" in {
      val fooFormat                  = Json.format[Foo]
      val strFormat: OFormat[String] = fooFormat.bimap(_.name, Foo(_))

      val expectedRepr = Json.obj("name" -> "bar")

      strFormat.reads(expectedRepr).mustEqual(JsSuccess("bar"))

      strFormat.writes("bar").mustEqual(expectedRepr)
    }
  }

  "Representation" should {
    "not be found by default" in {
      "implicitly[_root_.play.api.libs.json.Format.Representation[java.time.LocalDateTime, _root_.play.api.libs.json.JsString]]"
        .mustNot(typeCheck)

      "implicitly[_root_.play.api.libs.json.Format.Representation[java.time.LocalDate, _root_.play.api.libs.json.JsString]]"
        .mustNot(typeCheck)

      "implicitly[_root_.play.api.libs.json.Format.Representation[java.time.LocalTime, _root_.play.api.libs.json.JsString]]"
        .mustNot(typeCheck)

      "implicitly[_root_.play.api.libs.json.Format.Representation[java.time.OffsetDateTime, _root_.play.api.libs.json.JsString]]"
        .mustNot(typeCheck)

      "implicitly[_root_.play.api.libs.json.Format.Representation[java.time.OffsetTime, _root_.play.api.libs.json.JsString]]"
        .mustNot(typeCheck)

      "implicitly[_root_.play.api.libs.json.Format.Representation[java.time.ZonedDateTime, _root_.play.api.libs.json.JsString]]"
        .mustNot(typeCheck)

      "implicitly[_root_.play.api.libs.json.Format.Representation[java.time.Instant, _root_.play.api.libs.json.JsString]]"
        .mustNot(typeCheck)

      "implicitly[_root_.play.api.libs.json.Format.Representation[java.time.Duration, _root_.play.api.libs.json.JsString]]"
        .mustNot(typeCheck)

      "implicitly[_root_.play.api.libs.json.Format.Representation[scala.concurrent.duration.FiniteDuration, _root_.play.api.libs.json.JsString]]"
        .mustNot(typeCheck)
    }
  }

  "Map Format" should {
    "be resolved" when {
      "using Reads'able keys represented as JSON string (e.g. URI) as success" in {
        val key = "https://www.playframework.com/documentation/2.8.x/api/scala/index.html#play.api.libs.json.JsResult"

        implicitly[OFormat[Map[java.net.URI, Int]]]
      }

      "using key not represented as JSON string (tuple keys) as failure" in {
        "implicitly[_root_.play.api.libs.json.Format[Map[(Int, Int), String]]]".mustNot(typeCheck)

        "implicitly[_root_.play.api.libs.json.OFormat[Map[(Int, Int), String]]]".mustNot(typeCheck)
      }
    }
  }

  case class Foo(name: String)
}
