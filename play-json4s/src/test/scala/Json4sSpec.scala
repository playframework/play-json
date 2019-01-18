package tests // Keep it outside

import play.api.libs.json.{
  JsArray,
  JsBoolean,
  JsFalse,
  JsNull,
  JsNumber,
  JsValue,
  JsString,
  Json,
  JsSuccess,
  JsTrue,
  OFormat,
  Reads,
  Writes
}

import play.api.libs.json.json4s._

import org.json4s.{
  Formats => JFormats,
  JArray,
  JBool,
  JDecimal,
  JDouble,
  JField,
  JInt,
  JLong,
  JNull,
  JNothing,
  JObject,
  JSet,
  JString,
  JValue,
  Reader,
  Writer
}

final class Json4sSpec extends org.specs2.mutable.Specification {
  "JSON4S" title

  "Typeclass converters" should {
    "support Foo case class" >> {
      // Doesn't compile before the appropriate implicits are imported ...
      shapeless.test.illTyped("implicitly[Writer[Foo]]")
      shapeless.test.illTyped("implicitly[Reader[Foo]]")

      // ... there:
      import Formats._

      val foo = Foo("bar", 12)

      val jo = JObject(List(
        "name" -> JString("bar"),
        "lorem" -> JDecimal(BigDecimal(12))))

      "as Writer" in {
        val w: Writer[Foo] = implicitly[Writer[Foo]]

        w.getClass.getName must startWith("play.api.libs.json.json4s") and {
          JFormats.write(foo)(w) must_=== jo
        } and {
          JFormats.write(foo) must_=== jo
        }
      }

      "as Reader" in {
        val r: Reader[Foo] = implicitly[Reader[Foo]]

        r.getClass.getName must startWith("play.api.libs.json.json4s") and {
          JFormats.read(jo)(r) must_=== foo
        } and {
          JFormats.read[Foo](jo) must_=== foo
        }
      }
    }

    "support Bar class" >> {
      // Doesn't compile before the appropriate implicits are imported ...
      shapeless.test.illTyped("implicitly[Writes[Bar]]")
      shapeless.test.illTyped("implicitly[Reads[Bar]]")

      // ... there:
      import Formats._

      val bar = new Bar("foo", 1.23D)
      val jo = Json.obj("title" -> "foo", "score" -> 1.23D)

      "as Writes" in {
        val w: Writes[Bar] = implicitly[Writes[Bar]]

        shapeless.test.illTyped("implicitly[OWrites[Bar]]")
        // ... as there is no OWrites equivalent in JSON4S

        w.getClass.getName must startWith("play.api.libs.json.json4s") and {
          Json.toJson(bar) must_=== jo
        } and {
          Json.toJson(bar)(w) must_=== jo
        }
      }

      "as Reads" in {
        val r: Reads[Bar] = implicitly[Reads[Bar]]

        r.getClass.getName must startWith("play.api.libs.json.json4s") and {
          jo.validate(r) must_=== JsSuccess(bar)
        } and {
          jo.validate[Bar] must_=== JsSuccess(bar)
        }
      }
    }
  }

  "Value converters" should {
    // Doesn't compile before the appropriate conversions are imported ...
    shapeless.test.illTyped("implicitly[JValue](JsTrue)")

    // ... there:
    import JValueConverters._

    "handle JBool" in {
      converterTest[JBool, JsBoolean](JBool.True, JsTrue) and {
        converterTest[JBool, JsBoolean](JBool.False, JsFalse)
      }
    }

    "handle JNull" in {
      converterTest(JNull, JsNull) and {
        converterTest[JValue, JsValue](JNull, JsNull)
      }
    }

    "handle JNothing" in {
      // Doesn't provide specific conversion JNothing => JsNull
      shapeless.test.illTyped("implicitly[JNothing.type](JsNull)")

      implicitly[JsNull.type](JNothing) must_=== JsNull and {
        implicitly[JsValue](JNothing: JValue) must_== JsNull
      }
    }

    "handle JString" in {
      converterTest(JString("foo"), JsString("foo")) and {
        converterTest[JValue, JsValue](JString("foo"), JsString("foo"))
      }
    }

    "handle JDecimal" in {
      val dec = BigDecimal(1.2345D)

      converterTest(JDecimal(dec), JsNumber(dec)) and {
        converterTest[JValue, JsValue](JDecimal(dec), JsNumber(dec))
      }
    }

    "handle JArray" in {
      val jarr = JArray(List(
        JString("bar"), JDecimal(BigDecimal(12)), JNull))

      val jsa = Json.arr("bar", 12, JsNull)

      converterTest(jarr, jsa) and {
        converterTest[JValue, JsValue](jarr, jsa)
      }
    }

    "handle JSet" in {
      val set = JSet(Set(JString("bar"), JDecimal(BigDecimal(12))))
      val arr = Json.arr("bar", 12)

      // Doesn't provide specific conversion JSet => JsArray
      shapeless.test.illTyped("implicitly[JSet](arr)")

      implicitly[JsArray](set) must_=== arr and {
        implicitly[JsValue](set: JValue) must_=== arr
      }
    }

    "handle JObject" in {
      val jobj = JObject(List(
        "foo" -> JString("bar"),
        "lorem" -> JDecimal(BigDecimal(12)),
        "ipsum" -> JNull))

      val jso = Json.obj(
        "foo" -> "bar",
        "lorem" -> 12,
        "ipsum" -> JsNull)

      converterTest(jobj, jso) and {
        converterTest[JValue, JsValue](jobj, jso)
      }
    }

    "handle JInt" in {
      val jint = JInt(123)
      val jnum = JsNumber(123)

      // Doesn't provide specific conversion JInt => JsNumber
      shapeless.test.illTyped("implicitly[JInt](jnum)")

      implicitly[JsNumber](jint) must_=== jnum and {
        implicitly[JsValue](jint: JValue) must_=== jnum
      }
    }

    "handle JLong" in {
      val jlong = JLong(123)
      val jnum = JsNumber(123)

      // Doesn't provide specific conversion JLong => JsNumber
      shapeless.test.illTyped("implicitly[JLong](jnum)")

      implicitly[JsNumber](jlong) must_=== jnum and {
        implicitly[JsValue](jlong: JValue) must_=== jnum
      }
    }

    "handle JDouble" in {
      val jdouble = JDouble(123)
      val jnum = JsNumber(123)

      // Doesn't provide specific conversion JDouble => JsNumber, ...
      shapeless.test.illTyped("implicitly[JDouble](jnum)")

      implicitly[JsNumber](jdouble) must_=== jnum and {
        implicitly[JsValue](jdouble: JValue) must_=== jnum
      }
    }

    "handle JField" in {
      val dec = BigDecimal(12.345D)
      val jfield = "foo" -> JDecimal(dec)
      val field = "foo" -> JsNumber(dec)

      implicitly[(String, JsValue)](jfield) must_=== field and {
        implicitly[JField](field) must_=== jfield
      }
    }
  }

  // ---

  private def converterTest[A <: JValue, B <: JsValue](jv: A, js: B)(implicit a2b: A => B, b2a: B => A) = a2b(jv) must_=== js and (b2a(js) must_=== jv)
}

// ---

case class Foo(
  name: String,
  lorem: Int)

object Foo {
  implicit lazy val jsFormat: OFormat[Foo] = Json.format[Foo]
}

final class Bar(val title: String, val score: Double) {
  override def equals(that: Any): Boolean = that match {
    case other: Bar => (title, score) == (other.title, other.score)
    case _ => false
  }

  override def hashCode: Int = (title, score).hashCode
}

object Bar {
  implicit val reader: Reader[Bar] = new Reader[Bar] {
    def read(v: JValue): Bar = v match {
      case JObject(
        ("title", JString(title)) :: (
          "score", JDecimal(score)) :: _) =>
        new Bar(title, score.toDouble)

      case JObject(("score", JDecimal(score)) :: (
        "title", JString(title)) :: _) =>
        new Bar(title, score.toDouble)

      case _ => sys.error("unsupported")
    }
  }

  implicit val writer: Writer[Bar] = new Writer[Bar] {
    def write(bar: Bar): JValue = JObject(List(
      "title" -> JString(bar.title),
      "score" -> JDecimal(BigDecimal(bar.score))
    ))
  }
}
