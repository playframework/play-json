/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import java.math.BigInteger
import java.util.Calendar
import java.util.Date
import java.util.TimeZone

import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.ObjectMapper
import play.api.libs.functional.syntax._
import play.api.libs.json.Json._
import play.api.libs.json.jackson.JacksonJson

class JsonSpec extends org.specs2.mutable.Specification {
  "JSON".title

  import java.text.SimpleDateFormat
  val dateFormat = "yyyy-MM-dd'T'HH:mm:ssX" // Iso8601 format (forgot timezone stuff)
  val dateParser = new SimpleDateFormat(dateFormat)

  case class Post(body: String, created_at: Option[Date])

  case class BigNumbers(bigInt: BigInt, bigDec: BigDecimal)
  case class IntNumbers(long: Long, integer: Int)
  case class FloatNumbers(float: Float, double: Double)

  val exceedsDigitsLimit: BigDecimal         = BigDecimal("9" * 1000000)
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

  implicit val BigNumbersFormat: Format[BigNumbers]     = Json.format[BigNumbers]
  implicit val IntNumbersFormat: Format[IntNumbers]     = Json.format[IntNumbers]
  implicit val FloatNumbersFormat: Format[FloatNumbers] = Json.format[FloatNumbers]

  implicit val PostFormat: Format[Post] = (
    (__ \ Symbol("body")).format[String] and
      (__ \ Symbol("created_at"))
        .formatNullable[Option[Date]](
          Format(
            Reads.optionWithNull(Reads.dateReads(dateFormat)),
            Writes.optionWithNull(Writes.dateWrites(dateFormat))
          )
        )
        .inmap(optopt => optopt.flatten, (opt: Option[Date]) => Some(opt))
  )(Post.apply, p => (p.body, p.created_at))

  val LenientPostFormat: Format[Post] = (
    (__ \ Symbol("body")).format[String] and
      (__ \ Symbol("created_at")).formatNullable[Date](
        Format(
          Reads.IsoDateReads,
          Writes.dateWrites(dateFormat)
        )
      )
  )(Post.apply, p => (p.body, p.created_at))

  val mapper = new ObjectMapper()

  val preserveZeroDecimal: JsonConfig = {
    val defaultSerializerSettings = JsonConfig.settings.bigDecimalSerializerConfig
    val defaultParserSettings     = JsonConfig.settings.bigDecimalParseConfig
    val serializerSettings = BigDecimalSerializerConfig(
      defaultSerializerSettings.minPlain,
      defaultSerializerSettings.maxPlain,
      preserveZeroDecimal = true
    )

    JsonConfig(defaultParserSettings, serializerSettings)
  }

  def withJsonConfig[T](jsonConfig: JsonConfig)(f: () => T) = {
    try {
      JacksonJson.setConfig(jsonConfig)
      f.apply()
    } catch {
      case err: Throwable => throw err
    } finally {
      JacksonJson.setConfig(JsonConfig.settings)
    }
  }

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

      Json.parse(postJson).as[Post].aka("parsed").must_==(expectedPost)
    }

    "with default/lenient date format" should {
      "with default/lenient date format with millis and UTC zone" in {
        val postJson =
          """{"body": "foobar", "created_at": "2011-04-22T13:33:48.000Z"}"""
        val expectedPost = Post("foobar", Some(postDate))

        Json.parse(postJson).as[Post](LenientPostFormat).aka("parsed").must_==(expectedPost)
      }

      "with default/lenient date format with millis and ISO8601 zone" in {
        val cal = Calendar.getInstance(TimeZone.getTimeZone("UTC"))
        cal.setTime(postDate)
        cal.add(Calendar.HOUR_OF_DAY, -5)

        val postJson =
          """{"body": "foobar", "created_at": "2011-04-22T13:33:48.000+0500"}"""
        val expectedPost = Post("foobar", Some(cal.getTime))

        Json.parse(postJson).as[Post](LenientPostFormat).aka("parsed").must_==(expectedPost)
      }

      "with default/lenient date format with no millis and UTC zone" in {
        val postJson =
          """{"body": "foobar", "created_at": "2011-04-22T13:33:48Z"}"""
        val expectedPost = Post("foobar", Some(postDate))

        Json.parse(postJson).as[Post](LenientPostFormat).aka("parsed").must_==(expectedPost)
      }

      "with default/lenient date format with no millis and ISO8601 zone" in {
        val cal = Calendar.getInstance(TimeZone.getTimeZone("UTC"))
        cal.setTime(postDate)
        cal.add(Calendar.HOUR_OF_DAY, -7)

        val postJson =
          """{"body": "foobar", "created_at": "2011-04-22T13:33:48+0700"}"""
        val expectedPost = Post("foobar", Some(cal.getTime))

        Json.parse(postJson).as[Post](LenientPostFormat).aka("parsed").must_==(expectedPost)
      }

      "with default/lenient date format with millis" in {
        val postJson =
          """{"body": "foobar", "created_at": "2011-04-22T13:33:48.000"}"""
        val expectedPost = Post("foobar", Some(postDateWithTZ(TimeZone.getDefault)))

        Json.parse(postJson).as[Post](LenientPostFormat).aka("parsed").must_==(expectedPost)
      }

      "with default/lenient date format without millis or time zone" in {
        val postJson =
          """{"body": "foobar", "created_at": "2011-04-22T13:33:48"}"""
        val expectedPost = Post("foobar", Some(postDateWithTZ(TimeZone.getDefault)))

        Json.parse(postJson).as[Post](LenientPostFormat).aka("parsed").must_==(expectedPost)
      }
    }

    "when parsing numbers" in {
      def intsJson(long: String = "1", int: String = "1") = {
        s"""
           |{
           |  "long": $long,
           |  "integer": $int
           |}""".stripMargin
      }

      def floatsJson(float: String = "1.0", double: String = "1.0") = {
        s"""
           |{
           |  "float": $float,
           |  "double": $double
           |}""".stripMargin
      }

      def bigNumbersJson(bigDec: String, bigInt: String = BigInt(1).toString) = {
        s"""
           |{
           |  "bigInt": $bigInt,
           |  "bigDec": $bigDec
           |}""".stripMargin
      }

      "for Long" should {
        "success for valid positive number" in {
          Json.parse(intsJson(long = 123.toString)).as[IntNumbers].long.mustEqual(123)
        }

        "success for valid negative number" in {
          Json.parse(intsJson(long = -123.toString)).as[IntNumbers].long.mustEqual(-123)
        }

        "success for max value" in {
          Json.parse(intsJson(long = Long.MaxValue.toString)).as[IntNumbers].long.mustEqual(Long.MaxValue)
        }

        "success for min value" in {
          Json.parse(intsJson(long = Long.MinValue.toString)).as[IntNumbers].long.mustEqual(Long.MinValue)
        }

        "fail for positive number out of Long limits" in {
          val outOfLimits = BigDecimal(Long.MaxValue) + 10
          Json.parse(intsJson(long = outOfLimits.toString())).as[IntNumbers].must(throwA[JsResultException])
        }

        "fail for negative number out of Long limits" in {
          val outOfLimits = BigDecimal(Long.MaxValue) + 10
          Json.parse(intsJson(long = outOfLimits.unary_-.toString())).as[IntNumbers].must(throwA[JsResultException])
        }
      }

      "for Integer" should {
        "success for valid positive number" in {
          Json.parse(intsJson(int = 123.toString)).as[IntNumbers].integer.mustEqual(123)
        }

        "success for valid negative number" in {
          Json.parse(intsJson(int = -123.toString)).as[IntNumbers].integer.mustEqual(-123)
        }

        "fail for positive number out of Int limits" in {
          val outOfLimits = BigDecimal(Int.MaxValue) + 10
          Json.parse(intsJson(int = outOfLimits.toString())).as[IntNumbers].must(throwA[JsResultException])
        }

        "fail for negative number out of Int limits" in {
          val outOfLimits = BigDecimal(Int.MaxValue) + 10
          Json.parse(intsJson(int = outOfLimits.unary_-.toString())).as[IntNumbers].must(throwA[JsResultException])
        }
      }

      "for Float" should {
        "success for valid positive number" in {
          Json.parse(floatsJson(123.123.toString)).as[FloatNumbers].float.mustEqual(123.123F)
        }

        "success for valid negative number" in {
          Json.parse(floatsJson(float = -123.123.toString)).as[FloatNumbers].float.mustEqual(-123.123F)
        }

        "success for max value" in {
          val maxFloat = BigDecimal(Float.MaxValue.toString)
          Json.parse(floatsJson(float = maxFloat.toString())).as[FloatNumbers].float.mustEqual(Float.MaxValue)
        }

        "success for min value" in {
          val minFloat = BigDecimal(Float.MinValue.toString)
          Json.parse(floatsJson(float = minFloat.toString())).as[FloatNumbers].float.mustEqual(Float.MinValue)
        }
      }

      "for Double" should {
        "success for valid positive number" in {
          Json.parse(floatsJson(double = 123.123.toString)).as[FloatNumbers].double.mustEqual(123.123D)
        }

        "success for valid negative number" in {
          Json.parse(floatsJson(double = -123.123.toString)).as[FloatNumbers].double.mustEqual(-123.123D)
        }

        "success when parsing max value" in {
          val maxDouble = BigDecimal(Double.MaxValue)
          Json.parse(floatsJson(double = maxDouble.toString())).as[FloatNumbers].double.mustEqual(Double.MaxValue)
        }

        "success when parsing min value" in {
          val minDouble = BigDecimal(Double.MinValue)
          Json.parse(floatsJson(double = minDouble.toString)).as[FloatNumbers].double.mustEqual(Double.MinValue)
        }
      }

      "for BigDecimals" should {
        val parserSettings = JsonParserSettings.settings

        // note: precision refers to `JacksonJson.BigDecimalLimits.DefaultMathContext.getPrecision`
        "maintain precision when parsing BigDecimals within precision limit" in {
          val n    = BigDecimal("12345678901234567890.123456789")
          val json = toJson(n)
          parse(stringify(json)).mustEqual(json)
        }

        // note: precision refers to `JacksonJson.BigDecimalLimits.DefaultMathContext.getPrecision`
        "truncate when exceeding the precision limit" in {
          // last two "3" are exceeding 34 precision limit
          val n       = BigDecimal("10.1234567890123456789012345678901233")
          val numbers = Json.parse(bigNumbersJson(bigDec = n.toString)).as[BigNumbers]

          // Without the last two "3" since they were truncated ("...1233" becomes "...12")
          numbers.bigDec.mustEqual(BigDecimal("10.12345678901234567890123456789012"))
        }

        "drop trailing zeros for non-zero decimal by default" in {
          val s = stringify(toJson(BigDecimal("1.020300")))
          s.mustEqual("1.0203")
        }

        "drop single trailing zero decimal by default" in {
          val s = stringify(toJson(BigDecimal("1.0")))
          s.mustEqual("1")
        }

        "drop multiple trailing zero decimals by default" in {
          val s = stringify(toJson(BigDecimal("1.00")))
          s.mustEqual("1")
        }

        "drop multiple trailing zero decimals from zero value by default" in {
          val s = stringify(toJson(BigDecimal("0.00")))
          s.mustEqual("0")
        }

        "drop multiple trailing zero decimals from multiple of ten" in {
          val s = stringify(toJson(BigDecimal("10.00")))
          s.mustEqual("10")
        }

        "integer multiple of ten unchanged" in {
          val s = stringify(toJson(BigDecimal("10")))
          s.mustEqual("10")
        }

        "integer zero unchanged" in {
          val s = withJsonConfig(preserveZeroDecimal)(() => stringify(toJson(BigDecimal("0"))))
          s.mustEqual("0")
        }

        "drop multiple trailing zeros for non-zero decimal with preserveZeroDecimal=true" in {
          val s = withJsonConfig(preserveZeroDecimal)(() => stringify(toJson(BigDecimal("1.020300"))))
          s.mustEqual("1.0203")
        }

        "do not drop single trailing zero decimal with preserveZeroDecimal=true" in {
          val s = withJsonConfig(preserveZeroDecimal)(() => stringify(toJson(BigDecimal("1.0"))))
          s.mustEqual("1.0")
        }

        "preserve a single trailing zero decimal with preserveZeroDecimal=true" in {
          val s = withJsonConfig(preserveZeroDecimal)(() => stringify(toJson(BigDecimal("1.00"))))
          s.mustEqual("1.0")
        }

        "preserve a single trailing zero decimal from zero decimal with preserveZeroDecimal=true" in {
          val s = withJsonConfig(preserveZeroDecimal)(() => stringify(toJson(BigDecimal("0.00"))))
          s.mustEqual("0.0")
        }

        "preserve a single trailing zero decimal from multiple of ten with preserveZeroDecimal=true" in {
          val s = withJsonConfig(preserveZeroDecimal)(() => stringify(toJson(BigDecimal("10.00"))))
          s.mustEqual("10.0")
        }

        "integer multiple of ten with preserveZeroDecimal=true" in {
          val s = withJsonConfig(preserveZeroDecimal)(() => stringify(toJson(BigDecimal("10"))))
          s.mustEqual("10")
        }

        "integer zero with preserveZeroDecimal=true" in {
          val s = withJsonConfig(preserveZeroDecimal)(() => stringify(toJson(BigDecimal("0"))))
          s.mustEqual("0")
        }

        "success when not exceeding the scale limit for positive numbers" in {
          val withinScaleLimit = BigDecimal(2, parserSettings.bigDecimalParseSettings.scaleLimit - 1)
          Json
            .parse(bigNumbersJson(bigDec = withinScaleLimit.toString))
            .as[BigNumbers]
            .bigDec
            .mustEqual(withinScaleLimit)
        }

        "success when not exceeding the scale limit for negative numbers" in {
          val withinScaleLimitNegative = BigDecimal(2, parserSettings.bigDecimalParseSettings.scaleLimit - 1).unary_-
          Json.parse(bigNumbersJson(bigDec = withinScaleLimitNegative.toString)).as[BigNumbers].bigDec.mustEqual {
            withinScaleLimitNegative
          }
        }

        "success when not exceeding the number of digits limit for negative numbers" in {
          val withinDigitsLimitNegative = BigDecimal(Long.MinValue)
          Json
            .parse(bigNumbersJson(bigDec = withinDigitsLimitNegative.toString))
            .as[BigNumbers]
            .bigDec
            .mustEqual(withinDigitsLimitNegative)
        }

        "success when not exceeding the number of digits limit for positive numbers" in {
          val withinDigitsLimit = BigDecimal(Long.MaxValue)
          Json
            .parse(bigNumbersJson(bigDec = withinDigitsLimit.toString))
            .as[BigNumbers]
            .bigDec
            .mustEqual(withinDigitsLimit)
        }

        "fail when exceeding the scale limit for positive numbers" in {
          val exceedsScaleLimit = BigDecimal(2, parserSettings.bigDecimalParseSettings.scaleLimit + 1)
          Json
            .parse(bigNumbersJson(bigDec = exceedsScaleLimit.toString))
            .as[BigNumbers]
            .must(throwA[IllegalArgumentException])
        }

        "fail when exceeding the scale limit for negative numbers" in {
          val exceedsScaleLimit = BigDecimal(2, parserSettings.bigDecimalParseSettings.scaleLimit + 1).unary_-
          Json
            .parse(bigNumbersJson(bigDec = exceedsScaleLimit.toString))
            .as[BigNumbers]
            .must(throwA[IllegalArgumentException])
        }

        "fail when exceeding the number of digits limit for positive numbers" in {
          Json.parse(invalidJsonExceedingNumberOfDigits).as[BigNumbers].must(throwA[IllegalArgumentException])
        }

        "fail when exceeding the number of digits limit for negative numbers" in {
          Json.parse(invalidJsonExceedingNumberOfDigitsNegative).as[BigNumbers].must(throwA[IllegalArgumentException])
        }
      }
    }

    "Optional parameters in JSON should generate post w/o date" in {
      val postJson     = """{"body": "foobar"}"""
      val expectedPost = Post("foobar", None)
      Json.parse(postJson).as[Post].must(equalTo(expectedPost))
    }

    "Invalid parameters shoud be ignored" in {
      val postJson     = """{"body": "foobar", "created_at":null}"""
      val expectedPost = Post("foobar", None)
      Json.parse(postJson).as[Post].must(equalTo(expectedPost))
    }

    "Serialize and deserialize Jackson ObjectNodes" in {
      val on = mapper
        .createObjectNode()
        .put("foo", 1)
        .put("bar", "two")
      val json = Json.obj("foo" -> 1, "bar" -> "two")

      toJson(on).must_==(json) and (
        fromJson[JsonNode](json).map(_.toString).must_==(JsSuccess(on.toString))
      )
    }

    "Serialize and deserialize Jackson ArrayNodes" in {
      val an = mapper
        .createArrayNode()
        .add("one")
        .add(2)
      val json = Json.arr("one", 2)
      toJson(an).must(equalTo(json)) and (
        fromJson[JsonNode](json).map(_.toString).must_==(JsSuccess(an.toString))
      )
    }

    "Deserialize integer JsNumber as Jackson number node" in {
      val jsNum = JsNumber(new java.math.BigDecimal("50"))
      fromJson[JsonNode](jsNum).map(_.toString).must_==(JsSuccess("50"))
    }

    "Deserialize float JsNumber as Jackson number node" in {
      val jsNum = JsNumber(new java.math.BigDecimal("12.345"))
      fromJson[JsonNode](jsNum).map(_.toString).must_==(JsSuccess("12.345"))
    }

    "Serialize JsNumbers with integers correctly" in {
      val numStrings = Seq(
        "0",
        "1",
        "-1",
        Int.MaxValue.toString,
        Int.MinValue.toString,
        Long.MaxValue.toString,
        Long.MinValue.toString,
        BigInteger.valueOf(Long.MaxValue).add(BigInteger.ONE).toString,
        BigInteger.valueOf(Long.MinValue).add(BigInteger.valueOf(-1)).toString
      )
      numStrings.map { numString =>
        val bigDec = new java.math.BigDecimal(numString)
        Json.stringify(JsNumber(bigDec)).must_==(bigDec.toString)
      }
    }

    "Serialize JsNumbers with decimal points correctly" in {
      val numStrings = Seq(
        "0.123",
        "1.23456789",
        "-1.23456789",
        Float.MaxValue.toString,
        Float.MinValue.toString,
        Double.MaxValue.toString,
        Double.MinValue.toString,
        java.math.BigDecimal.valueOf(Double.MaxValue).add(java.math.BigDecimal.valueOf(1)).toString,
        java.math.BigDecimal.valueOf(Double.MinValue).add(java.math.BigDecimal.valueOf(-1)).toString
      )
      numStrings.map { numString =>
        val bigDec = new java.math.BigDecimal(numString)
        Json.stringify(JsNumber(bigDec)).must_==(bigDec.toString)
      }
    }

    "Serialize JsNumbers with e notation correctly" in {
      val numStrings = Seq(
        "1.23456789012345679012345679e999",
        "-1.23456789012345679012345679e999",
        "1.23456789012345679012345679e-999",
        "-1.23456789012345679012345679e-999"
      )
      numStrings.map { numString =>
        val bigDec = new java.math.BigDecimal(numString)
        Json.stringify(JsNumber(bigDec)).must_==(bigDec.toString)
      }
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

      Json.parse(stream).mustEqual(js)
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
      val parsed         = Json.parse(originalString)
      parsed.asInstanceOf[JsObject].fields.mustEqual(original.fields)
      Json.stringify(parsed).mustEqual(originalString)
    }
  }
}
