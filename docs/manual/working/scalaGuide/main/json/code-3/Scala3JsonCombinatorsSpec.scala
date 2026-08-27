/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package scalaguide.json

import org.specs2.mutable.Specification

final class Scala2JsonCombinatorsSpec extends Specification {
  "Scala 2 JSON" should {
    "allow creating Writes for model (using unlifted unapply)" in {
      //#scala3-writes-model
      import play.api.libs.json._
      import play.api.libs.functional.syntax._

      implicit val locationWrites: Writes[Location] = (
        (JsPath \ "lat").write[Double] and
          (JsPath \ "long").write[Double]
      )(Tuple.fromProductTyped(_: Location))

      implicit val residentWrites: Writes[Resident] = (
        (JsPath \ "name").write[String] and
          (JsPath \ "age").write[Int] and
          (JsPath \ "role").writeNullable[String]
      )(Tuple.fromProductTyped(_: Resident))

      implicit val placeWrites: Writes[Place] = (
        (JsPath \ "name").write[String] and
          (JsPath \ "location").write[Location] and
          (JsPath \ "residents").write[Seq[Resident]]
      )(Tuple.fromProductTyped(_: Place))

      val place = Place(
        "Watership Down",
        Location(51.235685, -1.309197),
        Seq(
          Resident("Fiver", 4, None),
          Resident("Bigwig", 6, Some("Owsla"))
        )
      )

      val json = Json.toJson(place)
      //#scala3-writes-model

      (json \ "name").get must_=== JsString("Watership Down")
    }

  }

  // ---

  case class Location(lat: Double, long: Double)
  case class Resident(name: String, age: Int, role: Option[String])
  case class Place(name: String, location: Location, residents: Seq[Resident])
}
