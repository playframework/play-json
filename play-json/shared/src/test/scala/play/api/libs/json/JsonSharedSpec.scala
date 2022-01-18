/*
 * Copyright (C) 2009-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import play.api.libs.functional.syntax._

import scala.collection.immutable.ListMap

import org.scalacheck.Gen
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class JsonSharedSpec extends AnyWordSpec with Matchers with org.scalatestplus.scalacheck.ScalaCheckPropertyChecks {
  case class User(id: Long, name: String, friends: List[User])

  implicit val UserFormat: Format[User] = (
    (__ \ Symbol("id")).format[Long] and
      (__ \ Symbol("name")).format[String] and
      (__ \ Symbol("friends")).lazyFormat(Reads.list(UserFormat), Writes.list(UserFormat))
  )(User.apply, u => (u.id, u.name, u.friends))

  case class Car(id: Long, models: Map[String, String])

  implicit val CarFormat: Format[Car] = (
    (__ \ Symbol("id")).format[Long] and
      (__ \ Symbol("models")).format[Map[String, String]]
  )(Car.apply, c => (c.id, c.models))

  def json[T](f: JsonFacade => T) = forAll(Gen.oneOf(Json, Json.configured, Json.using[Json.MacroOptions]))(f)

  "JSON" should {
    "equals JsObject independently of field order" in json { js =>
      js.obj(
        "field1" -> 123,
        "field2" -> "beta",
        "field3" -> js.obj(
          "field31" -> true,
          "field32" -> 123.45,
          "field33" -> js.arr("blabla", 456L, JsNull)
        )
      ).mustEqual(
        js.obj(
          "field2" -> "beta",
          "field3" -> js.obj(
            "field31" -> true,
            "field33" -> js.arr("blabla", 456L, JsNull),
            "field32" -> 123.45
          ),
          "field1" -> 123
        )
      )

      js.obj(
        "field1" -> 123,
        "field2" -> "beta",
        "field3" -> js.obj(
          "field31" -> true,
          "field32" -> 123.45,
          "field33" -> js.arr("blabla", JsNull)
        )
      ).must(not)
        .equal(
          js.obj(
            "field2" -> "beta",
            "field3" -> js.obj(
              "field31" -> true,
              "field33" -> js.arr("blabla", 456L),
              "field32" -> 123.45
            ),
            "field1" -> 123
          )
        )

      js.obj(
        "field1" -> 123,
        "field2" -> "beta",
        "field3" -> js.obj(
          "field31" -> true,
          "field32" -> 123.45,
          "field33" -> js.arr("blabla", 456L, JsNull)
        )
      ).must(not)
        .equal(
          js.obj(
            "field3" -> js.obj(
              "field31" -> true,
              "field33" -> js.arr("blabla", 456L, JsNull),
              "field32" -> 123.45
            ),
            "field1" -> 123
          )
        )
    }

    "support basic array operations" in json { js =>
      val names = js.arr("Luigi", "Kinopio", "Yoshi", "Mario")

      names.head.asOpt[String].mustEqual(Some("Luigi"))
      names(0).asOpt[String].mustEqual(Some("Luigi"))
      names(3).asOpt[String].mustEqual(Some("Mario"))
      names.last.asOpt[String].mustEqual(Some("Mario"))
      names.tail.toOption.mustEqual(Some(Json.arr("Kinopio", "Yoshi", "Mario")))

      val empty = js.arr()

      empty.head.toOption.mustEqual(None)
      empty.tail.toOption.mustEqual(None)
    }

    "serialize and deserialize maps properly" in json { js =>
      val c = Car(1, Map("ford" -> "1954 model"))

      js.toJson(c).as[Car].mustEqual(c)
    }

    "serialize and deserialize" in json { js =>
      val luigi   = User(1, "Luigi", List())
      val kinopio = User(2, "Kinopio", List())
      val yoshi   = User(3, "Yoshi", List())
      val mario   = User(0, "Mario", List(luigi, kinopio, yoshi))

      js.toJson(mario).as[User].mustEqual(mario)
    }

    "convert to a json object" in json { js =>
      val peach                  = User(1, "Peach", List())
      val writes: Writes[User]   = Json.writes[User]
      val owrites: OWrites[User] = Json.writes[User]

      Predef.assert(js.toJsObject(peach)(owrites).isInstanceOf[JsObject])
      js.toJsObject(peach)(owrites).mustEqual(js.toJson(peach)(writes))
      "js.toJsObject(1)".mustNot(typeCheck)
      "js.toJsObject(peach)(writes)".mustNot(typeCheck)
    }

    "convert to a byte array containing the UTF-8 representation" in json { js =>
      val json = js.parse("""
                            |{
                            |  "name": "coffee",
                            |  "symbol": "☕",
                            |  "price": "2.5 €"
                            |}
        """.stripMargin)
      val bytes      = js.toBytes(json)
      val string     = new String(bytes, "UTF-8")
      val parsedJson = js.parse(string)

      (parsedJson \ "symbol").mustEqual(JsDefined(JsString("☕")))
      (parsedJson \ "price").mustEqual(JsDefined(JsString("2.5 €")))
    }
  }

  "Complete JSON should create full object" when {
    "serialize long integers correctly" in json { js =>
      val t     = 1330950829160L
      val m     = Map("timestamp" -> t)
      val jsonM = js.toJson(m)

      (jsonM \ "timestamp").as[Long].mustEqual(t)
      jsonM.toString.mustEqual("""{"timestamp":1330950829160}""")
    }

    "serialize short integers correctly" in json { js =>
      val s: Short = 1234
      val m        = Map("s" -> s)
      val jsonM    = js.toJson(m)

      (jsonM \ "s").as[Short].mustEqual(s)
      jsonM.toString.mustEqual("""{"s":1234}""")
    }

    "serialize bytes correctly" in json { js =>
      val b: Byte = 123
      val m       = Map("b" -> b)
      val jsonM   = js.toJson(m)

      (jsonM \ "b").as[Byte].mustEqual(b)
      jsonM.toString.mustEqual("""{"b":123}""")
    }

    "serialize and deserialize BigDecimals" in json { js =>
      val n    = BigDecimal("12345678901234567890.42")
      val json = js.toJson(n)

      json.mustEqual(JsNumber(n))
      js.fromJson[BigDecimal](json).mustEqual(JsSuccess(n))
    }

    "write BigDecimals with large exponents in scientific notation" in json { js =>
      val n          = BigDecimal("1.2e1000")
      val jsonString = js.stringify(js.toJson(n))
      jsonString.mustEqual("1.2E+1000")
    }

    "write negative BigDecimals with large exponents in scientific notation" in json { js =>
      val n          = BigDecimal("-2.5e1000")
      val jsonString = js.stringify(js.toJson(n))
      jsonString.mustEqual("-2.5E+1000")
    }

    "write BigDecimals with large negative exponents in scientific notation" in json { js =>
      val n          = BigDecimal("6.75e-1000")
      val jsonString = js.stringify(js.toJson(n))
      jsonString.mustEqual("6.75E-1000")
    }

    "write BigDecimals with small exponents as a plain string" in json { js =>
      val n          = BigDecimal("1.234e3")
      val jsonString = js.stringify(js.toJson(n))
      jsonString.mustEqual("1234")
    }

    "write BigDecimals with small negative exponents as a plain string" in json { js =>
      val n          = BigDecimal("1.234e-3")
      val jsonString = js.stringify(js.toJson(n))
      jsonString.mustEqual("0.001234")
    }

    "write BigDecimals with integer base" in json { js =>
      val n          = BigDecimal("2e128")
      val jsonString = js.stringify(js.toJson(n))
      jsonString.mustEqual("2E+128")
    }

    "serialize and deserialize Lists" in json { js =>
      val xs: List[Int] = 1.to(5).toList
      val json          = js.arr(1, 2, 3, 4, 5)

      js.toJson(xs).mustEqual(json)
      js.fromJson[List[Int]](json).mustEqual(JsSuccess(xs))
    }

    "Map[String,String] should be turned into JsValue" in json { js =>
      js.toJson(Map("k" -> "v")).toString.mustEqual("""{"k":"v"}""")
    }

    "can parse recursive object" in json { js =>
      val recursiveJson = """{"foo": {"foo":["bar"]}, "bar": {"foo":["bar"]}}"""
      val expectedJson = JsObject(
        List(
          "foo" -> JsObject(
            List(
              "foo" -> JsArray(Array[JsValue](JsString("bar")))
            )
          ),
          "bar" -> JsObject(
            List(
              "foo" -> JsArray(Array[JsValue](JsString("bar")))
            )
          )
        )
      )

      js.parse(recursiveJson).mustEqual(expectedJson)
    }

    "can parse null values in Object" in json { js =>
      js.parse("""{"foo": null}""").mustEqual(JsObject(List("foo" -> JsNull)))
    }

    "can parse null values in Array" in json { js =>
      js.parse("[null]").mustEqual(JsArray(Array(JsNull)))
    }

    "null root object should be parsed as JsNull" in json { js =>
      js.parse("null").mustEqual(JsNull)
    }

    "JSON pretty print" in json { js =>
      def jo = js.obj(
        "key1" -> "toto",
        "key2" -> js.obj("key21" -> "tata", "key22" -> 123),
        "key3" -> js.arr(1, "tutu")
      )

      js.prettyPrint(jo)
        .replace("\r\n", "\n")
        .mustEqual("""{
  "key1" : "toto",
  "key2" : {
    "key21" : "tata",
    "key22" : 123
  },
  "key3" : [ 1, "tutu" ]
}""")
    }

    "asciiStringify should escape non-ascii characters" in json { js =>
      def jo = js.obj(
        "key1" -> "\u2028\u2029\u2030",
        "key2" -> "\u00E1\u00E9\u00ED\u00F3\u00FA",
        "key3" -> "\u00A9\u00A3",
        "key4" -> "\u6837\u54C1"
      )

      js.asciiStringify(jo)
        .mustEqual(
          "{\"key1\":\"\\u2028\\u2029\\u2030\"," +
            "\"key2\":\"\\u00E1\\u00E9\\u00ED\\u00F3\\u00FA\"," +
            "\"key3\":\"\\u00A9\\u00A3\"," + "" +
            "\"key4\":\"\\u6837\\u54C1\"}"
        )
    }

    "asciiStringify should escape ascii characters properly" in json { js =>
      def jo = Json.obj(
        "key1" -> "ab\n\tcd",
        "key2" -> "\"\r"
      )

      js.asciiStringify(jo).mustEqual("""{"key1":"ab\n\tcd","key2":"\"\r"}""")
    }
  }

  "JSON Writes" should {
    "write list/seq/set/map" in json { js =>
      import Writes._

      js.toJson(List(1, 2, 3)).mustEqual(js.arr(1, 2, 3))
      js.toJson(Set("alpha", "beta", "gamma")).mustEqual(js.arr("alpha", "beta", "gamma"))
      js.toJson(Seq("alpha", "beta", "gamma")).mustEqual(js.arr("alpha", "beta", "gamma"))
      js.toJson(Map("key1" -> "value1", "key2" -> "value2")).mustEqual(js.obj("key1" -> "value1", "key2" -> "value2"))

      implicit val myWrites: OWrites[(List[Int], Set[String], Seq[String], Map[String, String])] = (
        (__ \ Symbol("key1")).write(constraints.list[Int]) and
          (__ \ Symbol("key2")).write(constraints.set[String]) and
          (__ \ Symbol("key3")).write(constraints.seq[String]) and
          (__ \ Symbol("key4")).write(constraints.map[String])
      ).tupled

      js.toJson(
        (
          List(1, 2, 3),
          Set("alpha", "beta", "gamma"),
          Seq("alpha", "beta", "gamma"),
          Map("key1" -> "value1", "key2" -> "value2")
        )
      ).mustEqual(
        js.obj(
          "key1" -> js.arr(1, 2, 3),
          "key2" -> js.arr("alpha", "beta", "gamma"),
          "key3" -> js.arr("alpha", "beta", "gamma"),
          "key4" -> js.obj("key1" -> "value1", "key2" -> "value2")
        )
      )
    }

    "write in 2nd level" in json { js =>
      case class TestCase(id: String, attr1: String, attr2: String)

      def jo = Json.obj(
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
      )(t => (t.id, t.attr1, t.attr2))

      js.toJson(TestCase("my-id", "foo", "bar")).mustEqual(jo)
    }

    "keep the insertion order on ListMap".taggedAs(UnstableInScala213) in json { js =>
      def test = js.toJson(
        ListMap(
          "name" -> "foo",
          "zip"  -> "foo",
          "city" -> "foo"
        )
      )
      val req = """{"name":"foo", "zip":"foo", "city":"foo"}"""

      test.toString.mustEqual(js.parse(req).toString)
    // must be(equalIgnoringSpace(Json.parse(req).toString))
    }

    "keep insertion order on large ListMap".taggedAs(UnstableInScala213) in json { js =>
      def test = js.toJson(
        ListMap(
          "name"      -> "a",
          "zip"       -> "foo",
          "city"      -> "foo",
          "address"   -> "foo",
          "phone"     -> "foo",
          "latitude"  -> "foo",
          "longitude" -> "foo",
          "hny"       -> "foo",
          "hz"        -> "foo",
          "hek"       -> "foo",
          "hev"       -> "foo",
          "kny"       -> "foo",
          "kz"        -> "foo",
          "kek"       -> "foo",
          "kev"       -> "foo",
          "szeny"     -> "foo",
          "szez"      -> "foo",
          "szeek"     -> "foo",
          "szeev"     -> "foo",
          "csny"      -> "foo",
          "csz"       -> "foo",
          "csek"      -> "foo",
          "csev"      -> "foo",
          "pny"       -> "foo",
          "pz"        -> "foo",
          "pek"       -> "foo",
          "pev"       -> "foo",
          "szony"     -> "foo",
          "szoz"      -> "foo",
          "szoek"     -> "foo",
          "szoev"     -> "foo",
          "vny"       -> "foo",
          "vz"        -> "foo",
          "vek"       -> "foo",
          "vev"       -> "foo"
        )
      )

      def req =
        """{"name": "a", "zip": "foo", "city": "foo", "address": "foo", "phone": "foo", "latitude": "foo", "longitude": "foo", "hny": "foo", "hz": "foo", "hek": "foo", "hev": "foo", "kny": "foo", "kz": "foo", "kek": "foo", "kev": "foo", "szeny": "foo", "szez": "foo", "szeek": "foo", "szeev": "foo", "csny": "foo", "csz": "foo", "csek": "foo", "csev": "foo", "pny": "foo", "pz": "foo", "pek": "foo", "pev": "foo", "szony": "foo", "szoz": "foo", "szoek": "foo", "szoev": "foo", "vny": "foo", "vz": "foo", "vek": "foo", "vev": "foo"}"""

      test.toString.mustEqual(js.parse(req).toString) // ).ignoreSpace
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
