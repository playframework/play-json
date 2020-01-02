/*
 * Copyright (C) Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import org.scalatest._

class WritesSharedSpec extends WordSpec with MustMatchers {
  "Functional Writes" should {
    implicit val locationWrites = Writes[Location] { location =>
      Json.obj(
        "lat"  -> location.lat,
        "long" -> location.long
      )
    }

    "be successful for the simple case class Location" in {
      Json
        .toJson(Location(0.123d, 0.456d))
        .mustEqual(
          Json.obj(
            "lat"  -> 0.123d,
            "long" -> 0.456d
          )
        )
    }

    "be contramap'ed" in {
      val w = implicitly[Writes[String]]
      val ow = OWrites[String] { str =>
        Json.obj("string" -> str)
      }

      w.contramap[Int](_.toString).writes(1).mustEqual(JsString("1"))

      ow.contramap[Char](_.toString).writes('A').mustEqual(Json.obj("string" -> "A"))
    }
  }

  "Traversable Writes" should {
    "write Seqs" in {
      Json.toJson(Seq(5, 4, 3, 2, 1)).mustEqual(Json.arr(5, 4, 3, 2, 1))
    }

    "write SortedSets" in {
      import scala.collection.immutable.SortedSet
      Json.toJson(SortedSet(1, 2, 3, 4, 5)).mustEqual(Json.arr(1, 2, 3, 4, 5))
    }

    "write mutable SortedSets" in {
      import scala.collection.mutable.SortedSet
      Json.toJson(SortedSet(1, 2, 3, 4, 5)).mustEqual(Json.arr(1, 2, 3, 4, 5))
    }
  }

  "Map Writes" should {
    "write lazy maps" in {
      Json.toJson(Map("a" -> 1).map(kv => kv._1 -> (kv._2 + 1))).mustEqual(Json.obj("a" -> 2))
    }
  }

  "Iterable writes" should {
    "write maps" in {
      Json.toJson(Map(1 -> "one")).mustEqual(Json.arr(Json.arr(1, "one")))
    }
  }

  "Big integer Writes" should {
    "write as JsNumber" in {
      val jsNum = JsNumber(BigDecimal("123"))

      Json.toJson(BigInt("123")).mustEqual(jsNum)
      Json.toJson(new java.math.BigInteger("123")).mustEqual(jsNum)
    }
  }

  "EnumFormat" should {
    import TestEnums.EnumWithCustomNames._
    import TestEnums.EnumWithDefaultNames._

    "serialize correctly enum with custom names" in {
      Json.toJson(customEnum1).mustEqual(JsString("ENUM1"))
      Json.toJson(customEnum2).mustEqual(JsString("ENUM2"))
    }

    "serialize correctly enum with default names" in {
      Json.toJson(defaultEnum1).mustEqual(JsString("defaultEnum1"))
      Json.toJson(defaultEnum2).mustEqual(JsString("defaultEnum2"))
    }
  }

  "URI" should {
    "be written as string" in {
      val strRepr = "https://www.playframework.com/documentation/2.6.x/api/scala/index.html#play.api.libs.json.JsResult"

      Json.toJson(new java.net.URI(strRepr)).mustEqual(JsString(strRepr))
    }
  }

  // ---

  case class Location(lat: Double, long: Double)
}
