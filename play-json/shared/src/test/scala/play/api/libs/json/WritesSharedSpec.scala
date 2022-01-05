/*
 * Copyright (C) 2009-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

final class WritesSharedSpec extends AnyWordSpec with Matchers {
  "Functional Writes" should {
    implicit val locationWrites: Writes[Location] = Writes[Location] { location =>
      Json.obj(
        "lat"  -> location.lat,
        "long" -> location.long
      )
    }

    "be successful for the simple case class Location" in {
      Json
        .toJson(Location(0.123D, 0.456D))
        .mustEqual(
          Json.obj(
            "lat"  -> 0.123D,
            "long" -> 0.456D
          )
        )
    }

    "be contramap'ed" in {
      val w  = implicitly[Writes[String]]
      val ow = OWrites[String] { str => Json.obj("string" -> str) }

      w.contramap[Int](_.toString).writes(1).mustEqual(JsString("1"))

      val owc: OWrites[Char] = ow.contramap[Char](_.toString)

      owc.writes('A').mustEqual(Json.obj("string" -> "A"))
    }

    "be narrow'ed" when {
      "simple JSON value" in {
        val w = implicitly[Writes[JsValue]]

        w.narrow[JsString].writes(JsString("foo")).mustEqual(JsString("foo"))
        w.narrow[JsNumber].writes(JsNumber(2D)).mustEqual(JsNumber(2D))
      }

      "JSON object" in {
        trait Foo {
          def bar: String
        }
        class Lorem(val bar: String) extends Foo

        val ow                  = OWrites[Foo] { foo => Json.obj("bar" -> foo.bar) }
        val owc: OWrites[Lorem] = ow.narrow[Lorem]

        owc.writes(new Lorem("ipsum")).mustEqual(Json.obj("bar" -> "ipsum"))
      }
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
      Json.toJson(Map(1 -> "one")).mustEqual(Json.obj("1" -> "one"))
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

    "serialize correctly enum with custom names" in {
      Json.toJson(customEnum1).mustEqual(JsString("ENUM1"))
      Json.toJson(customEnum2).mustEqual(JsString("ENUM2"))
    }
  }

  "URI" should {
    "be written as string" in {
      val strRepr = "https://www.playframework.com/documentation/2.6.x/api/scala/index.html#play.api.libs.json.JsResult"

      Json.toJson(new java.net.URI(strRepr)).mustEqual(JsString(strRepr))
    }
  }

  "Identity writes" should {
    import scala.reflect.ClassTag
    import scala.language.higherKinds

    def success[T <: JsValue, W[A] <: Writes[A]](fixture: T)(implicit
        w: W[T],
        ct: ClassTag[T],
        wt: ClassTag[W[T]]
    ) =
      s"be resolved as ${wt.runtimeClass.getSimpleName}[${ct.runtimeClass.getSimpleName}] for $fixture" in {
        w.writes(fixture).mustEqual(fixture)
      }

    success[JsArray, Writes](Json.arr("foo", 2))
    success[JsValue, Writes](Json.arr("foo", 2))

    success[JsBoolean, Writes](JsTrue)
    success[JsValue, Writes](JsFalse)

    success[JsNull.type, Writes](JsNull)
    success[JsValue, Writes](JsNull)

    success[JsNumber, Writes](JsNumber(1))
    success[JsValue, Writes](JsNumber(1))

    success[JsObject, Writes](JsObject(Map("foo" -> JsNumber(1))))
    success[JsValue, Writes](JsObject(Map("foo" -> JsNumber(1))))
    success[JsObject, OWrites](JsObject(Map("foo" -> JsNumber(1))))

    success[JsString, Writes](JsString("foo"))
    success[JsValue, Writes](JsString("foo"))
  }

  // ---

  case class Location(lat: Double, long: Double)
}
