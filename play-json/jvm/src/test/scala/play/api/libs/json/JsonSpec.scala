/*
 * Copyright (C) 2009-2016 Lightbend Inc. <https://www.lightbend.com>
 */
package play.api.libs.json

import java.util.{ Calendar, Date, TimeZone }

import com.fasterxml.jackson.databind.{ JsonNode, ObjectMapper }
import play.api.libs.functional.syntax._
import play.api.libs.json.Json._

import scala.collection.immutable.ListMap

class JsonSpec extends org.specs2.mutable.Specification {
  "JSON" title

  import java.text.SimpleDateFormat
  val dateFormat = "yyyy-MM-dd'T'HH:mm:ssX" // Iso8601 format (forgot timezone stuff)
  val dateParser = new SimpleDateFormat(dateFormat)

  case class Post(body: String, created_at: Option[Date])

  implicit val PostFormat: Format[Post] = (
    (__ \ 'body).format[String] and
    (__ \ 'created_at).formatNullable[Option[Date]](
      Format(
        Reads.optionWithNull(Reads.dateReads(dateFormat)),
        Writes.optionWithNull(Writes.dateWrites(dateFormat))
      )
    ).inmap(optopt => optopt.flatten, (opt: Option[Date]) => Some(opt))
  )(Post, unlift(Post.unapply))

  val LenientPostFormat: Format[Post] = (
    (__ \ 'body).format[String] and
    (__ \ 'created_at).formatNullable[Date](
      Format(
        Reads.IsoDateReads,
        Writes.dateWrites(dateFormat)
      )
    )
  )(Post, unlift(Post.unapply))

  val mapper = new ObjectMapper()

  "Complete JSON should create full object" >> {
    lazy val postDate: Date = dateParser.parse("2011-04-22T13:33:48Z")
    def postDateWithTZ(tz: TimeZone): Date = {
      val cal = Calendar.getInstance
      cal.setTime(postDate)
      cal.add(Calendar.MILLISECOND, -1 * tz.getOffset(postDate.getTime))
      cal.getTime
    }

    "with custom date format" in {
      val postJson =
        """{"body": "foobar", "created_at": "2011-04-22T13:33:48Z"}"""
      val expectedPost = Post("foobar", Some(postDate))

      Json.parse(postJson).as[Post] aka "parsed" must_== expectedPost
    }

    "with default/lenient date format with millis and UTC zone" in {
      val postJson =
        """{"body": "foobar", "created_at": "2011-04-22T13:33:48.000Z"}"""
      val expectedPost = Post("foobar", Some(postDate))

      Json.parse(postJson).as[Post](LenientPostFormat).
        aka("parsed") must_== expectedPost
    }

    "with default/lenient date format with millis and ISO8601 zone" in {
      val cal = Calendar.getInstance(TimeZone getTimeZone "UTC")
      cal.setTime(postDate)
      cal.add(Calendar.HOUR_OF_DAY, -5)

      val postJson =
        """{"body": "foobar", "created_at": "2011-04-22T13:33:48.000+0500"}"""
      val expectedPost = Post("foobar", Some(cal.getTime))

      Json.parse(postJson).as[Post](LenientPostFormat).
        aka("parsed") must_== expectedPost
    }

    "with default/lenient date format with no millis and UTC zone" in {
      val postJson =
        """{"body": "foobar", "created_at": "2011-04-22T13:33:48Z"}"""
      val expectedPost = Post("foobar", Some(postDate))

      Json.parse(postJson).as[Post](LenientPostFormat).
        aka("parsed") must_== expectedPost
    }

    "with default/lenient date format with no millis and ISO8601 zone" in {
      val cal = Calendar.getInstance(TimeZone getTimeZone "UTC")
      cal.setTime(postDate)
      cal.add(Calendar.HOUR_OF_DAY, -7)

      val postJson =
        """{"body": "foobar", "created_at": "2011-04-22T13:33:48+0700"}"""
      val expectedPost = Post("foobar", Some(cal.getTime))

      Json.parse(postJson).as[Post](LenientPostFormat).
        aka("parsed") must_== expectedPost
    }

    "with default/lenient date format with millis" in {
      val postJson =
        """{"body": "foobar", "created_at": "2011-04-22T13:33:48.000"}"""
      val expectedPost = Post("foobar", Some(postDateWithTZ(TimeZone.getDefault)))

      Json.parse(postJson).as[Post](LenientPostFormat).
        aka("parsed") must_== expectedPost
    }

    "with default/lenient date format without millis or time zone" in {
      val postJson =
        """{"body": "foobar", "created_at": "2011-04-22T13:33:48"}"""
      val expectedPost = Post("foobar", Some(postDateWithTZ(TimeZone.getDefault)))

      Json.parse(postJson).as[Post](LenientPostFormat).
        aka("parsed") must_== expectedPost
    }

    "Optional parameters in JSON should generate post w/o date" in {
      val postJson = """{"body": "foobar"}"""
      val expectedPost = Post("foobar", None)
      Json.parse(postJson).as[Post] must equalTo(expectedPost)
    }

    "Invalid parameters shoud be ignored" in {
      val postJson = """{"body": "foobar", "created_at":null}"""
      val expectedPost = Post("foobar", None)
      Json.parse(postJson).as[Post] must equalTo(expectedPost)
    }

    "Serialize and deserialize Jackson ObjectNodes" in {
      val on = mapper.createObjectNode()
        .put("foo", 1).put("bar", "two")
      val json = Json.obj("foo" -> 1, "bar" -> "two")

      toJson(on) must_== json and (
        fromJson[JsonNode](json).map(_.toString) must_== JsSuccess(on.toString)
      )
    }

    "Serialize and deserialize Jackson ArrayNodes" in {
      val an = mapper.createArrayNode()
        .add("one").add(2)
      val json = Json.arr("one", 2)
      toJson(an) must equalTo(json) and (
        fromJson[JsonNode](json).map(_.toString) must_== JsSuccess(an.toString)
      )
    }

    "Deserialize integer JsNumber as Jackson number node" in {
      val jsNum = JsNumber(new java.math.BigDecimal("50"))
      fromJson[JsonNode](jsNum).map(_.toString) must_== JsSuccess("50")
    }

    "Deserialize float JsNumber as Jackson number node" in {
      val jsNum = JsNumber(new java.math.BigDecimal("12.345"))
      fromJson[JsonNode](jsNum).map(_.toString) must_== JsSuccess("12.345")
    }

    "parse from InputStream" in {
      val js = Json.obj(
        "key1" -> "value1",
        "key2" -> true,
        "key3" -> JsNull,
        "key4" -> Json.arr(1, 2.5, "value2", false, JsNull),
        "key5" -> Json.obj(
          "key6" -> "こんにちは",
          "key7" -> BigDecimal("12345678901234567890.123456789")
        )
      )
      def stream = new java.io.ByteArrayInputStream(
        js.toString.getBytes("UTF-8")
      )

      Json.parse(stream) mustEqual js
    }

    "not lose precision when parsing BigDecimals" in {
      val n = BigDecimal("12345678901234567890.123456789")
      val json = toJson(n)
      parse(stringify(json)) mustEqual json
    }

    "not lose precision when parsing big integers" in {
      // By big integers, we just mean integers that overflow long,
      // since Jackson has different code paths for them from decimals
      val json = toJson(BigDecimal("123456789012345678901234567890"))
      parse(stringify(json)) mustEqual json
    }

    "keep isomorphism between serialized and deserialized data" in {
      val original = Json.obj(
        "key1" -> "value1",
        "key2" -> true,
        "key3" -> JsNull,
        "key4" -> Json.arr(1, 2.5, "value2", false, JsNull),
        "key5" -> Json.obj(
          "key6" -> "こんにちは",
          "key7" -> BigDecimal("12345678901234567890.123456789")
        )
      )
      val originalString = Json.stringify(original)
      val parsed = Json.parse(originalString)
      parsed.asInstanceOf[JsObject].fields mustEqual original.fields
      Json.stringify(parsed) mustEqual originalString
    }
  }
}

