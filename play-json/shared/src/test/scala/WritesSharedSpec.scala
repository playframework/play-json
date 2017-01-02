/*
 * Copyright (C) 2009-2016 Lightbend Inc. <https://www.lightbend.com>
 */
package play.api.libs.json

import org.scalatest._

class WritesSharedSpec extends WordSpec with MustMatchers {
  "Functionnal Reads" should {
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
  }

  // ---

  case class Location(lat: Double, long: Double)
}
