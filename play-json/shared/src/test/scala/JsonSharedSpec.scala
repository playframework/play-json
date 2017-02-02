/*
 * Copyright (C) 2009-2016 Lightbend Inc. <https://www.lightbend.com>
 */
package play.api.libs.json

import play.api.libs.functional.syntax._
import play.api.libs.json.Json._

import scala.collection.immutable.ListMap

import org.scalatest._

class JsonSharedSpec extends WordSpec with MustMatchers {
  case class User(id: Long, name: String, friends: List[User])

  implicit val UserFormat: Format[User] = (
    (__ \ 'id).format[Long] and
    (__ \ 'name).format[String] and
    (__ \ 'friends).lazyFormat(Reads.list(UserFormat), Writes.list(UserFormat))
  )(User, unlift(User.unapply))

  case class Car(id: Long, models: Map[String, String])

  implicit val CarFormat = (
    (__ \ 'id).format[Long] and
    (__ \ 'models).format[Map[String, String]]
  )(Car, unlift(Car.unapply))

  "JSON" should {
    "equals JsObject independently of field order" in {
      Json.obj(
        "field1" -> 123,
        "field2" -> "beta",
        "field3" -> Json.obj(
          "field31" -> true,
          "field32" -> 123.45,
          "field33" -> Json.arr("blabla", 456L, JsNull)
        )
      ) mustEqual (
          Json.obj(
            "field2" -> "beta",
            "field3" -> Json.obj(
              "field31" -> true,
              "field33" -> Json.arr("blabla", 456L, JsNull),
              "field32" -> 123.45
            ),
            "field1" -> 123
          )
        )

      Json.obj(
        "field1" -> 123,
        "field2" -> "beta",
        "field3" -> Json.obj(
          "field31" -> true,
          "field32" -> 123.45,
          "field33" -> Json.arr("blabla", JsNull)
        )
      ) must not equal (
          Json.obj(
            "field2" -> "beta",
            "field3" -> Json.obj(
              "field31" -> true,
              "field33" -> Json.arr("blabla", 456L),
              "field32" -> 123.45
            ),
            "field1" -> 123
          )
        )

      Json.obj(
        "field1" -> 123,
        "field2" -> "beta",
        "field3" -> Json.obj(
          "field31" -> true,
          "field32" -> 123.45,
          "field33" -> Json.arr("blabla", 456L, JsNull)
        )
      ) must not equal (
          Json.obj(
            "field3" -> Json.obj(
              "field31" -> true,
              "field33" -> Json.arr("blabla", 456L, JsNull),
              "field32" -> 123.45
            ),
            "field1" -> 123
          )
        )
    }

    "support basic array operations" in {
      val names = Json.arr("Luigi", "Kinopio", "Yoshi", "Mario")
      names.head.asOpt[String] mustEqual Some("Luigi")
      names(0).asOpt[String] mustEqual Some("Luigi")
      names(3).asOpt[String] mustEqual Some("Mario")
      names.last.asOpt[String] mustEqual Some("Mario")
      names.tail.toOption mustEqual Some(Json.arr("Kinopio", "Yoshi", "Mario"))

      val empty = Json.arr()
      empty.head.toOption mustEqual None
      empty.tail.toOption mustEqual None
    }

    "serialize and deserialize maps properly" in {
      val c = Car(1, Map("ford" -> "1954 model"))
      val jsonCar = toJson(c)

      jsonCar.as[Car] mustEqual (c)
    }

    "serialize and deserialize" in {
      val luigi = User(1, "Luigi", List())
      val kinopio = User(2, "Kinopio", List())
      val yoshi = User(3, "Yoshi", List())
      val mario = User(0, "Mario", List(luigi, kinopio, yoshi))
      val jsonMario = toJson(mario)
      jsonMario.as[User] mustEqual (mario)
    }

    "convert to a byte array containing the UTF-8 representation" in {
      val json = Json.parse(
        """
          |{
          |  "name": "coffee",
          |  "symbol": "☕",
          |  "price": "2.5 €"
          |}
        """.stripMargin)
      val bytes = Json.toBytes(json)
      val string = new String(bytes, "UTF-8")
      val parsedJson = Json.parse(string)

      parsedJson \ "symbol" mustEqual JsDefined(JsString("☕"))
      parsedJson \ "price" mustEqual JsDefined(JsString("2.5 €"))
    }
  }

  "Complete JSON should create full object" when {
    "serialize long integers correctly" in {
      val t = 1330950829160L
      val m = Map("timestamp" -> t)
      val jsonM = toJson(m)

      (jsonM \ "timestamp").as[Long] mustEqual t
      jsonM.toString mustEqual """{"timestamp":1330950829160}"""
    }

    "serialize short integers correctly" in {
      val s: Short = 1234
      val m = Map("s" -> s)
      val jsonM = toJson(m)

      (jsonM \ "s").as[Short] mustEqual s
      jsonM.toString mustEqual """{"s":1234}"""
    }

    "serialize bytes correctly" in {
      val b: Byte = 123
      val m = Map("b" -> b)
      val jsonM = toJson(m)

      (jsonM \ "b").as[Byte] mustEqual b
      jsonM.toString mustEqual """{"b":123}"""
    }

    "serialize and deserialize BigDecimals" in {
      val n = BigDecimal("12345678901234567890.42")
      val json = toJson(n)

      json mustEqual (JsNumber(n))
      fromJson[BigDecimal](json) mustEqual JsSuccess(n)
    }

    "write BigDecimals with large exponents in scientific notation" in {
      val n = BigDecimal("1.2e1000")
      val jsonString = stringify(toJson(n))
      jsonString mustEqual "1.2E+1000"
    }

    "write negative BigDecimals with large exponents in scientific notation" in {
      val n = BigDecimal("-2.5e1000")
      val jsonString = stringify(toJson(n))
      jsonString mustEqual "-2.5E+1000"
    }

    "write BigDecimals with large negative exponents in scientific notation" in {
      val n = BigDecimal("6.75e-1000")
      val jsonString = stringify(toJson(n))
      jsonString mustEqual "6.75E-1000"
    }

    "write BigDecimals with small exponents as a plain string" in {
      val n = BigDecimal("1.234e3")
      val jsonString = stringify(toJson(n))
      jsonString mustEqual "1234"
    }

    "write BigDecimals with small negative exponents as a plain string" in {
      val n = BigDecimal("1.234e-3")
      val jsonString = stringify(toJson(n))
      jsonString mustEqual "0.001234"
    }

    "write BigDecimals with integer base" in {
      val n = BigDecimal("2e128")
      val jsonString = stringify(toJson(n))
      jsonString mustEqual "2E+128"
    }

    "serialize and deserialize Lists" in {
      val xs: List[Int] = (1 to 5).toList
      val json = arr(1, 2, 3, 4, 5)

      toJson(xs) mustEqual json
      fromJson[List[Int]](json) mustEqual JsSuccess(xs)
    }

    "Map[String,String] should be turned into JsValue" in {
      toJson(Map("k" -> "v")).toString mustEqual """{"k":"v"}"""
    }

    "can parse recursive object" in {
      val recursiveJson = """{"foo": {"foo":["bar"]}, "bar": {"foo":["bar"]}}"""
      val expectedJson = JsObject(List(
        "foo" -> JsObject(List(
          "foo" -> JsArray(List[JsValue](JsString("bar")))
        )),
        "bar" -> JsObject(List(
          "foo" -> JsArray(List[JsValue](JsString("bar")))
        ))
      ))
      Json.parse(recursiveJson) mustEqual (expectedJson)
    }

    "can parse null values in Object" in {
      Json.parse("""{"foo": null}""") mustEqual JsObject(List("foo" -> JsNull))
    }

    "can parse null values in Array" in {
      Json.parse("[null]") mustEqual JsArray(List(JsNull))
    }

    "null root object should be parsed as JsNull" in {
      parse("null") mustEqual JsNull
    }

    "JSON pretty print" in {
      val js = Json.obj(
        "key1" -> "toto",
        "key2" -> Json.obj("key21" -> "tata", "key22" -> 123),
        "key3" -> Json.arr(1, "tutu")
      )

      Json.prettyPrint(js) mustEqual ("""{
  "key1" : "toto",
  "key2" : {
    "key21" : "tata",
    "key22" : 123
  },
  "key3" : [ 1, "tutu" ]
}""")
    }

    "asciiStringify should escape non-ascii characters" in {
      val js = Json.obj(
        "key1" -> "\u2028\u2029\u2030",
        "key2" -> "\u00E1\u00E9\u00ED\u00F3\u00FA",
        "key3" -> "\u00A9\u00A3",
        "key4" -> "\u6837\u54C1"
      )

      Json.asciiStringify(js) mustEqual (
        "{\"key1\":\"\\u2028\\u2029\\u2030\"," +
        "\"key2\":\"\\u00E1\\u00E9\\u00ED\\u00F3\\u00FA\"," +
        "\"key3\":\"\\u00A9\\u00A3\"," + "" +
        "\"key4\":\"\\u6837\\u54C1\"}"
      )
    }

    "asciiStringify should escape ascii characters properly" in {
      def js = Json.obj(
        "key1" -> "ab\n\tcd",
        "key2" -> "\"\r"
      )

      Json.asciiStringify(js) mustEqual (
        """{"key1":"ab\n\tcd","key2":"\"\r"}"""
      )
    }
  }

  "JSON Writes" should {
    "write list/seq/set/map" in {
      import Writes._

      Json.toJson(List(1, 2, 3)) mustEqual (Json.arr(1, 2, 3))
      Json.toJson(Set("alpha", "beta", "gamma")) mustEqual (Json.arr("alpha", "beta", "gamma"))
      Json.toJson(Seq("alpha", "beta", "gamma")) mustEqual (Json.arr("alpha", "beta", "gamma"))
      Json.toJson(Map("key1" -> "value1", "key2" -> "value2")) mustEqual (Json.obj("key1" -> "value1", "key2" -> "value2"))

      implicit val myWrites = (
        (__ \ 'key1).write(constraints.list[Int]) and
        (__ \ 'key2).write(constraints.set[String]) and
        (__ \ 'key3).write(constraints.seq[String]) and
        (__ \ 'key4).write(constraints.map[String])
      ).tupled

      Json.toJson((
        List(1, 2, 3),
        Set("alpha", "beta", "gamma"),
        Seq("alpha", "beta", "gamma"),
        Map("key1" -> "value1", "key2" -> "value2")
      )) mustEqual (
        Json.obj(
          "key1" -> Json.arr(1, 2, 3),
          "key2" -> Json.arr("alpha", "beta", "gamma"),
          "key3" -> Json.arr("alpha", "beta", "gamma"),
          "key4" -> Json.obj("key1" -> "value1", "key2" -> "value2")
        )
      )
    }

    "write in 2nd level" in {
      case class TestCase(id: String, attr1: String, attr2: String)

      val js = Json.obj(
        "id" -> "my-id",
        "data" -> Json.obj(
          "attr1" -> "foo",
          "attr2" -> "bar"
        )
      )

      implicit val testCaseWrites: Writes[TestCase] = (
        (__ \ "id").write[String] and
        (__ \ "data" \ "attr1").write[String] and
        (__ \ "data" \ "attr2").write[String]
      )(unlift(TestCase.unapply))

      Json.toJson(TestCase("my-id", "foo", "bar")) mustEqual (js)
    }

    "keep the insertion order on ListMap" in {
      val test = Json.toJson(
        ListMap(
          "name" -> "foo",
          "zip" -> "foo",
          "city" -> "foo"
        )
      )
      val req = """{"name":"foo", "zip":"foo", "city":"foo"}"""

      test.toString mustEqual Json.parse(req).toString
      //must be(equalIgnoringSpace(Json.parse(req).toString))
    }

    "keep insertion order on large ListMap" in {
      def test = Json.toJson(
        ListMap(
          "name" -> "a", "zip" -> "foo", "city" -> "foo",
          "address" -> "foo", "phone" -> "foo", "latitude" -> "foo",
          "longitude" -> "foo", "hny" -> "foo", "hz" -> "foo",
          "hek" -> "foo", "hev" -> "foo", "kny" -> "foo",
          "kz" -> "foo", "kek" -> "foo", "kev" -> "foo",
          "szeny" -> "foo", "szez" -> "foo", "szeek" -> "foo",
          "szeev" -> "foo", "csny" -> "foo", "csz" -> "foo",
          "csek" -> "foo", "csev" -> "foo", "pny" -> "foo",
          "pz" -> "foo", "pek" -> "foo", "pev" -> "foo",
          "szony" -> "foo", "szoz" -> "foo", "szoek" -> "foo",
          "szoev" -> "foo", "vny" -> "foo", "vz" -> "foo",
          "vek" -> "foo", "vev" -> "foo"
        )
      )

      def req = """{"name": "a", "zip": "foo", "city": "foo", "address": "foo", "phone": "foo", "latitude": "foo", "longitude": "foo", "hny": "foo", "hz": "foo", "hek": "foo", "hev": "foo", "kny": "foo", "kz": "foo", "kek": "foo", "kev": "foo", "szeny": "foo", "szez": "foo", "szeek": "foo", "szeev": "foo", "csny": "foo", "csz": "foo", "csek": "foo", "csev": "foo", "pny": "foo", "pz": "foo", "pek": "foo", "pev": "foo", "szony": "foo", "szoz": "foo", "szoek": "foo", "szoev": "foo", "vny": "foo", "vz": "foo", "vek": "foo", "vev": "foo"}"""

      test.toString mustEqual Json.parse(req).toString //).ignoreSpace
    }
  }

  // ---

  def equalIgnoringSpace(x: String) = new IgnoreSpaceMatcher(x)

  class IgnoreSpaceMatcher(
      expected: String
  ) extends org.scalatest.matchers.BeMatcher[String] {

    val right = expected.replaceAll("\\s", "")

    def apply(left: String) = org.scalatest.matchers.MatchResult(
      left.replaceAll("\\s", "") == right,
      s"$left is not equal to $right",
      s"$left is equal to $right"
    )
  }
}
