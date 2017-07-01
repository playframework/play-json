/*
 * Copyright (C) 2009-2016 Lightbend Inc. <https://www.lightbend.com>
 */
package play.api.libs.json

import scala.concurrent.duration.{ Duration, FiniteDuration }

import org.scalatest._
import org.scalatest.prop.TableDrivenPropertyChecks._

class WritesSharedSpec extends WordSpec with MustMatchers {
  "Functionnal Writes" should {
    import play.api.libs.functional.syntax._

    implicit val locationWrites = Writes[Location] { location =>
      Json.obj(
        "lat" -> location.lat,
        "long" -> location.long
      )
    }

    "be successful for the simple case class Location" in {
      Json.toJson(Location(0.123D, 0.456D)) mustEqual Json.obj(
        "lat" -> 0.123D, "long" -> 0.456D
      )
    }

    "be successful for finite Duration" in forAll(Table(
      "duration" -> "json",
      Duration.Zero -> JsNumber(BigDecimal(0D)),
      FiniteDuration(1L, "second") -> JsString("1 second"),
      Duration("5seconds") -> JsString("5 seconds"))) { (duration, json) =>
      Json.toJson(duration) mustEqual json
    }

    "be successful as millis for FiniteDuration" in forAll(Table(
      "duration" -> "json",
      Duration.Zero -> 0L,
      FiniteDuration(1L, "second") -> 1000L
    )) { (duration, millis) =>
      Json.toJson(duration)(Writes.finiteDurationNumberWrites) mustEqual (
        JsNumber(BigDecimal(millis)))
    }

    "be successful for infinite Duration" in forAll(Table(
      "duration" -> "json",
      Duration.Inf -> "Inf",
      Duration.MinusInf -> "MinusInf",
      Duration.Undefined -> "Undefined"
    )) { (duration, json) =>
      Json.toJson(duration) mustEqual JsString(json)
    }
  }

  // ---

  case class Location(lat: Double, long: Double)
}
