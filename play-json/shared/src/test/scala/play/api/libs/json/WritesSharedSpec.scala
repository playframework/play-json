/*
 * Copyright (C) 2009-2018 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import org.scalatest._

class WritesSharedSpec extends WordSpec with MustMatchers {
  "Functional Writes" should {
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

  "Traversable Writes" should {
    "write Seqs" in {
      Json.toJson(Seq(5, 4, 3, 2, 1)) mustEqual Json.arr(5, 4, 3, 2, 1)
    }

    "write SortedSets" in {
      import scala.collection.immutable.SortedSet
      Json.toJson(SortedSet(1, 2, 3, 4, 5)) mustEqual Json.arr(1, 2, 3, 4, 5)
    }

    "write mutable SortedSets" in {
      import scala.collection.mutable.SortedSet
      Json.toJson(SortedSet(1, 2, 3, 4, 5)) mustEqual Json.arr(1, 2, 3, 4, 5)
    }
  }

  "Big integer Writes" should {
    "write as JsNumber" in {
      val jsNum = JsNumber(BigDecimal("123"))

      Json.toJson(BigInt("123")) mustEqual jsNum
      Json.toJson(new java.math.BigInteger("123")) mustEqual jsNum
    }
  }

  // ---

  case class Location(lat: Double, long: Double)
}
