/*
 * Copyright (C) 2009-2018 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import java.util.{ Calendar, Date, TimeZone }

import com.fasterxml.jackson.databind.{ JsonNode, ObjectMapper }
import play.api.libs.functional.syntax._
import play.api.libs.json.Json._
import play.api.libs.json.jackson.JacksonJson

class JsonSpec extends org.specs2.mutable.Specification {
  "JSON" title

  import java.text.SimpleDateFormat
  val dateFormat = "yyyy-MM-dd'T'HH:mm:ssX" // Iso8601 format (forgot timezone stuff)
  val dateParser = new SimpleDateFormat(dateFormat)

  case class Post(body: String, created_at: Option[Date])

  case class BigNumbers(bigInt: BigInt, bigDec: BigDecimal)
  case class IntNumbers(long: Long, integer: Int)
  case class FloatNumbers(float: Float, double: Double)

  val exceedsDigitsLimit: BigDecimal = BigDecimal("9" * 1000000)
  val exceedsDigitsLimitNegative: BigDecimal = exceedsDigitsLimit.unary_-

  val invalidJsonExceedingNumberOfDigits: String = s"""
    |{
    |  "bigInt": 1,
    |  "bigDec": $exceedsDigitsLimit
    |}""".stripMargin

  val invalidJsonExceedingNumberOfDigitsNegative: String = s"""
    |{
    |  "bigInt": 1,
    |  "bigDec": $exceedsDigitsLimitNegative
    |}""".stripMargin

  implicit val BigNumbersFormat: Format[BigNumbers] = Json.format[BigNumbers]
  implicit val IntNumbersFormat: Format[IntNumbers] = Json.format[IntNumbers]
  implicit val FloatNumbersFormat: Format[FloatNumbers] = Json.format[FloatNumbers]

  implicit val PostFormat: Format[Post] = (
    (__ \ 'body).format[String] and
    (__ \ 'created_at).formatNullable[Option[Date]](
      Format(
        Reads.optionWithNull(Reads.dateReads(dateFormat)),
        Writes.optionWithNull(Writes.dateWrites(dateFormat))
      )
    ).inmap(optopt => optopt.flatten, (opt: Option[Date]) => Some(opt))
  ) (Post, unlift(Post.unapply))

  val LenientPostFormat: Format[Post] = (
    (__ \ 'body).format[String] and
    (__ \ 'created_at).formatNullable[Date](
      Format(
        Reads.IsoDateReads,
        Writes.dateWrites(dateFormat)
      )
    )
  ) (Post, unlift(Post.unapply))

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

    "with default/lenient date format" should {
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
    }

    "when parsing numbers" in {
      "for Long" should {

        "success for valid positive number" in {
          Json.parse(
            s"""
              |{
              |  "long": 123,
              |  "integer": 1
              |}""".stripMargin)
            .as[IntNumbers].long mustEqual 123
        }

        "success for valid negative number" in {
          Json.parse(
            s"""
               |{
               |  "long": -123,
               |  "integer": 1
               |}""".stripMargin)
            .as[IntNumbers].long mustEqual -123
        }

        "success for max value" in {
          Json.parse(
            s"""
               |{
               |  "long": ${Long.MaxValue},
               |  "integer": 1
               |}""".stripMargin)
            .as[IntNumbers].long mustEqual Long.MaxValue
        }

        "success for min value" in {
          Json.parse(
            s"""
               |{
               |  "long": ${Long.MinValue},
               |  "integer": 1
               |}""".stripMargin)
            .as[IntNumbers].long mustEqual Long.MinValue
        }

        "fail for positive number out of Long limits" in {
          val outOfLimits = BigDecimal(Long.MaxValue) + 10
          Json.parse(
            s"""
               |{
               |  "long": $outOfLimits,
               |  "integer": 1
               |}""".stripMargin)
            .as[IntNumbers] must throwA[JsResultException]
        }

        "fail for negative number out of Long limits" in {
          val outOfLimits = BigDecimal(Long.MaxValue) + 10
          Json.parse(
            s"""
               |{
               |  "long": ${outOfLimits.unary_-},
               |  "integer": 1
               |}""".stripMargin)
            .as[IntNumbers] must throwA[JsResultException]
        }
      }

      "for Integer" should {

        "success for valid positive number" in {
          Json.parse(
            s"""
               |{
               |  "long": 1,
               |  "integer": 123
               |}""".stripMargin)
            .as[IntNumbers].integer mustEqual 123
        }

        "success for valid negative number" in {
          Json.parse(
            s"""
               |{
               |  "long": 1,
               |  "integer": -123
               |}""".stripMargin)
            .as[IntNumbers].integer mustEqual -123
        }

        "fail for positive number out of Int limits" in {
          val outOfLimits = BigDecimal(Int.MaxValue) + 10
          Json.parse(
            s"""
               |{
               |  "long": 1,
               |  "integer": $outOfLimits
               |}""".stripMargin)
            .as[IntNumbers] must throwA[JsResultException]
        }

        "fail for negative number out of Int limits" in {
          val outOfLimits = BigDecimal(Int.MaxValue) + 10
          Json.parse(
            s"""
               |{
               |  "long": 1,
               |  "integer": ${outOfLimits.unary_-}
               |}""".stripMargin)
            .as[IntNumbers] must throwA[JsResultException]
        }
      }

      "for Float" should {

        "success for valid positive number" in {
          Json.parse(
            s"""
               |{
               |  "float": 123.123,
               |  "double": 1
               |}""".stripMargin)
            .as[FloatNumbers].float mustEqual 123.123f
        }

        "success for valid negative number" in {
          Json.parse(
            s"""
               |{
               |  "float": -123.123,
               |  "double": 1
               |}""".stripMargin)
            .as[FloatNumbers].float mustEqual -123.123f
        }

        "success for max value" in {
          val outOfLimits = BigDecimal(Float.MaxValue.toString)
          Json.parse(
            s"""
               |{
               |  "float": $outOfLimits,
               |  "double": 1
               |}""".stripMargin)
            .as[FloatNumbers].float mustEqual Float.MaxValue
        }

        "success for min value" in {
          val outOfLimits = BigDecimal(Float.MinValue.toString)
          Json.parse(
            s"""
               |{
               |  "float": $outOfLimits,
               |  "double": 1
               |}""".stripMargin)
            .as[FloatNumbers].float mustEqual Float.MinValue
        }
      }

      "for Double" should {

        "success for valid positive number" in {
          Json.parse(
            s"""
               |{
               |  "float": 1,
               |  "double": 123.123
               |}""".stripMargin)
            .as[FloatNumbers].double mustEqual 123.123d
        }

        "success for valid negative number" in {
          Json.parse(
            s"""
               |{
               |  "float": 1,
               |  "double": -123.123
               |}""".stripMargin)
            .as[FloatNumbers].double mustEqual -123.123d
        }

        "success when parsing max value" in {
          val outOfLimits = BigDecimal(Double.MaxValue)
          Json.parse(
            s"""
               |{
               |  "float": 1,
               |  "double": $outOfLimits
               |}""".stripMargin)
            .as[FloatNumbers].double mustEqual Double.MaxValue
        }

        "success when parsing min value" in {
          val outOfLimits = BigDecimal(Double.MinValue)
          Json.parse(
            s"""
               |{
               |  "float": 1,
               |  "double": $outOfLimits
               |}""".stripMargin)
            .as[FloatNumbers].double mustEqual Double.MinValue
        }
      }

      "for BigDecimals" should {
        "maintain precision when parsing BigDecimals within precision limit" in {
          val n = BigDecimal("12345678901234567890.123456789")
          val json = toJson(n)
          parse(stringify(json)) mustEqual json
        }

        "truncate when exceeding the precision limit" in {
          // lsat two six are exceeding 34 precision limit
          val n = BigDecimal("10.12345678912345678912345678912345666")
          val numbers = Json.parse(
            s"""
               |{
               |  "bigInt": 1,
               |  "bigDec": $n
               |}""".stripMargin
          ).as[BigNumbers]

          // Without the last two "6" since they were truncated ("...4566" becomes "...46")
          numbers.bigDec mustEqual BigDecimal("10.12345678912345678912345678912346")
        }

        "success when not exceeding the scale limit for positive numbers" in {
          val withinScaleLimit = BigDecimal(2, JacksonJson.BigDecimalLimits.ScaleLimit - 1)
          Json.parse(
            s"""
                 |{
                 |  "bigInt": 1,
                 |  "bigDec": $withinScaleLimit
                 |}""".stripMargin
          ).as[BigNumbers].bigDec mustEqual withinScaleLimit
        }

        "success when not exceeding the scale limit for negative numbers" in {
          val withinScaleLimitNegative = BigDecimal(2, JacksonJson.BigDecimalLimits.ScaleLimit - 1).unary_-
          Json.parse(
            s"""
                 |{
                 |  "bigInt": 1,
                 |  "bigDec": $withinScaleLimitNegative
                 |}""".stripMargin
          ).as[BigNumbers].bigDec mustEqual { withinScaleLimitNegative }
        }

        "success when not exceeding the number of digits limit for negative numbers" in {
          val withinDigitsLimitNegative = BigDecimal(Long.MinValue)
          Json.parse(
            s"""
               |{
               |  "bigInt": 1,
               |  "bigDec": $withinDigitsLimitNegative

               |}""".
              stripMargin
          ).as[BigNumbers].bigDec mustEqual withinDigitsLimitNegative
        }

        "success when not exceeding the number of digits limit for positive numbers" in {
          val withinDigitsLimit = BigDecimal(Long.MaxValue)
          Json.parse(
            s"""
               |{
               |  "bigInt": 1,
               |  "bigDec": $withinDigitsLimit
               |}""".stripMargin
          ).as[BigNumbers].bigDec mustEqual withinDigitsLimit
        }

        "fail when exceeding the scale limit for positive numbers" in {
          val exceedsScaleLimit = BigDecimal(2, JacksonJson.BigDecimalLimits.ScaleLimit + 1)
          Json.parse(
            s"""
                 |{
                 |  "bigInt": 1,
                 |  "bigDec": $exceedsScaleLimit
                 |}""".stripMargin
          ).as[BigNumbers] must throwA[IllegalArgumentException]
        }

        "fail when exceeding the scale limit for negative numbers" in {
          val exceedsScaleLimit = BigDecimal(2, JacksonJson.BigDecimalLimits.ScaleLimit + 1).unary_-
          Json.parse(
            s"""
                 |{
                 |  "bigInt": 1,
                 |  "bigDec": $exceedsScaleLimit
                 |}""".stripMargin
          ).as[BigNumbers] must throwA[IllegalArgumentException]
        }

        "fail when exceeding the number of digits limit for positive numbers" in {
          Json.parse(invalidJsonExceedingNumberOfDigits).as[BigNumbers] must throwA[IllegalArgumentException]
        }

        "fail when exceeding the number of digits limit for negative numbers" in {
          Json.parse(invalidJsonExceedingNumberOfDigitsNegative).as[BigNumbers] must throwA[IllegalArgumentException]
        }
      }
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

