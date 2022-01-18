/*
 * Copyright (C) 2009-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import java.math.{ BigDecimal => JBigDec }
import java.util.Locale
import java.time.{ Duration => JDuration }
import java.time.Instant
import java.time.Period
import java.time.LocalDate
import java.time.LocalTime
import java.time.LocalDateTime
import java.time.OffsetDateTime
import java.time.ZoneId
import java.time.ZonedDateTime
import java.time.ZoneOffset
import java.time.temporal.ChronoUnit
import java.time.format.DateTimeFormatter

import org.specs2.specification.core.Fragment

class ReadsSpec extends org.specs2.mutable.Specification {
  val veryLargeNumber = BigDecimal("9" * 1000000)

  "JSON Reads".title

  "Local date/time" should {
    val DefaultReads = implicitly[Reads[LocalDateTime]]
    import DefaultReads.reads

    val CustomReads1 = Reads.localDateTimeReads("dd/MM/yyyy, HH:mm:ss")

    @inline def dateTime(input: String) =
      LocalDateTime.parse(input, DateTimeFormatter.ISO_DATE_TIME)

    lazy val correctedReads = Reads.localDateTimeReads(
      DateTimeFormatter.ISO_DATE_TIME,
      _.drop(1)
    )

    val CustomReads2 = Reads.localDateTimeReads(
      DateTimeFormatter.ofPattern("dd/MM/yyyy, HH:mm:ss"),
      _.drop(2)
    )

    "be successfully read from number" in {
      reads(JsNumber(JBigDec.valueOf(123L))).must_===(
        JsSuccess(
          LocalDateTime.ofInstant(
            Instant.ofEpochMilli(123L),
            ZoneOffset.UTC
          )
        )
      )
    }

    "not be read from invalid number" in {
      reads(JsNumber(BigDecimal("1000000000e1000000000"))).must(beLike { case JsError.Message("error.expected.long") =>
        ok
      })
    }

    "not be read from invalid string" in {
      reads(JsString("invalid")).must(beLike { case JsError.Message("error.expected.date.isoformat") =>
        ok
      })
    }

    "be successfully read with default implicit" >> {
      "from '2011-12-03T10:15:30'" in {
        reads(JsString("2011-12-03T10:15:30")).aka("read date").must_===(JsSuccess(dateTime("2011-12-03T10:15:30")))
      }

      "from '2011-12-03T10:15:30+01:00' (with TZ offset)" in {
        reads(JsString("2011-12-03T10:15:30+01:00"))
          .aka("read date")
          .must_===(JsSuccess(dateTime("2011-12-03T10:15:30+01:00")))
      }

      "from '2011-12-03T10:15:30+01:00[Europe/Paris]' (with time zone)" in {
        reads(JsString("2011-12-03T10:15:30+01:00[Europe/Paris]"))
          .aka("read date")
          .must_===(JsSuccess(dateTime("2011-12-03T10:15:30+01:00[Europe/Paris]")))
      }
    }

    "be successfully read with custom pattern from '03/12/2011, 10:15:30'" in {
      CustomReads1
        .reads(JsString("03/12/2011, 10:15:30"))
        .aka("read date")
        .must_===(JsSuccess(dateTime("2011-12-03T10:15:30")))
    }

    "not be read from invalid corrected string" >> {
      "with default implicit" in {
        correctedReads
          .reads(JsString("2011-12-03T10:15:30"))
          .must(beLike { case JsError.Message(key) =>
            key.must_===("error.expected.date.isoformat")
          })
      }

      "with custom formatter" in {
        CustomReads2
          .reads(JsString("03/12/2011, 10:15:30"))
          .must(beLike { case JsError.Message("error.expected.date.isoformat") =>
            ok
          })
      }
    }

    "be successfully read from corrected string" >> {
      lazy val time = dateTime("2011-12-03T10:15:30")

      "with default implicit" in {
        correctedReads.reads(JsString("_2011-12-03T10:15:30")).aka("read date").must_===(JsSuccess(time))
      }

      "with custom formatter" in {
        CustomReads2.reads(JsString("# 03/12/2011, 10:15:30")).aka("read date").must_===(JsSuccess(time))
      }
    }
  }

  "Offset date/time" should {
    val DefaultReads = implicitly[Reads[OffsetDateTime]]
    import DefaultReads.reads

    val CustomReads1 = Reads.offsetDateTimeReads("dd/MM/yyyy, HH:mm:ssXXX")

    @inline def dateTime(input: String) = OffsetDateTime.parse(input)

    lazy val correctedReads = Reads.offsetDateTimeReads(
      DateTimeFormatter.ISO_OFFSET_DATE_TIME,
      _.drop(1)
    )

    val CustomReads2 = Reads.offsetDateTimeReads(
      DateTimeFormatter.ofPattern("dd/MM/yyyy, HH:mm:ss ZZZ"),
      _.drop(2)
    )

    "not be read" >> {
      "from an invalid string" in {
        reads(JsString("invalid")).must(beLike { case JsError.Message("error.expected.date.isoformat") =>
          ok
        })
      }

      "from a number" in {
        reads(JsNumber(123L)).must(beLike { case JsError.Message("error.expected.date") =>
          ok
        })
      }
    }

    "be successfully read with default implicit" >> {
      "from '2011-12-03T10:15:30-05:00'" in {
        reads(JsString("2011-12-03T10:15:30-05:00"))
          .aka("read date")
          .must_===(JsSuccess(dateTime("2011-12-03T10:15:30-05:00")))
      }
    }

    "be successfully read with custom pattern from '03/12/2011, 10:15:30'" in {
      CustomReads1
        .reads(JsString("03/12/2011, 10:15:30-05:00"))
        .aka("read date")
        .must_===(JsSuccess(dateTime("2011-12-03T10:15:30-05:00")))
    }

    "not be read from invalid corrected string" >> {
      "with default implicit" in {
        correctedReads
          .reads(JsString("2011-12-03T10:15:30"))
          .must(beLike { case JsError.Message("error.expected.date.isoformat") =>
            ok
          })
      }

      "with custom formatter" in {
        CustomReads2
          .reads(JsString("03/12/2011, 10:15:30"))
          .must(beLike { case JsError.Message("error.expected.date.isoformat") =>
            ok
          })
      }
    }

    "be successfully read from corrected string" >> {
      lazy val time = dateTime("2011-12-03T10:15:30-05:00")

      "with default implicit" in {
        correctedReads.reads(JsString("_2011-12-03T10:15:30-05:00")).aka("read date").must_===(JsSuccess(time))
      }

      "with custom formatter" in {
        CustomReads2.reads(JsString("# 03/12/2011, 10:15:30 -0500")).aka("read date").must_===(JsSuccess(time))
      }
    }
  }

  "Zoned date/time" should {
    val DefaultReads = implicitly[Reads[ZonedDateTime]]
    import DefaultReads.reads

    val CustomReads1 = Reads.zonedDateTimeReads("dd/MM/yyyy, HH:mm:ssXXX")

    @inline def dateTime(input: String) = ZonedDateTime.parse(input)

    lazy val correctedReads = Reads.zonedDateTimeReads(
      DateTimeFormatter.ISO_DATE_TIME,
      _.drop(1)
    )

    val CustomReads2 = Reads.zonedDateTimeReads(
      DateTimeFormatter.ofPattern("dd/MM/yyyy, HH:mm:ssVV"),
      _.drop(2)
    )

    "be successfully read from number" in {
      reads(JsNumber(JBigDec.valueOf(123L))).must_===(
        JsSuccess(
          ZonedDateTime.ofInstant(
            Instant.ofEpochMilli(123L),
            ZoneOffset.UTC
          )
        )
      )
    }

    "not be read from invalid string" in {
      reads(JsString("invalid")).must(beLike { case JsError.Message("error.expected.date.isoformat") =>
        ok
      })
    }

    "not be read from invalid number" in {
      reads(JsNumber(BigDecimal("1000000000e1000000000")))
        .aka("read date")
        .must(beLike { case JsError.Message("error.expected.long") =>
          ok
        })
    }

    "be successfully read with default implicit" >> {
      "from '2011-12-03T10:15:30+01:00' (with TZ offset)" in {
        reads(JsString("2011-12-03T10:15:30+01:00"))
          .aka("read date")
          .must_===(JsSuccess(dateTime("2011-12-03T10:15:30+01:00")))
      }

      "from '2011-12-03T10:15:30+01:00[Europe/Paris]' (with time zone)" in {
        reads(JsString("2011-12-03T10:15:30+01:00[Europe/Paris]"))
          .aka("read date")
          .must_===(JsSuccess(dateTime("2011-12-03T10:15:30+01:00[Europe/Paris]")))
      }
    }

    "be successfully read with custom pattern from '03/12/2011, 10:15:30+08:00'" in {
      CustomReads1
        .reads(JsString("03/12/2011, 10:15:30+08:00"))
        .aka("read date")
        .must_===(JsSuccess(dateTime("2011-12-03T10:15:30+08:00")))
    }

    "not be read from invalid corrected string" >> {
      "with default implicit" in {
        correctedReads
          .reads(JsString("2011-12-03T10:15:30"))
          .must(beLike { case JsError.Message("error.expected.date.isoformat") =>
            ok
          })
      }

      "with custom formatter" in {
        CustomReads2
          .reads(JsString("03/12/2011, 10:15:30+08:00"))
          .must(beLike { case JsError.Message("error.expected.date.isoformat") =>
            ok
          })
      }
    }

    "be successfully read from corrected string" >> {
      lazy val time = dateTime("2011-12-03T10:15:30+08:00")

      "with default implicit" in {
        correctedReads.reads(JsString("_2011-12-03T10:15:30+08:00")).aka("read date").must_===(JsSuccess(time))
      }

      "with custom formatter" in {
        CustomReads2.reads(JsString("# 03/12/2011, 10:15:30+08:00")).aka("read date").must_===(JsSuccess(time))
      }
    }
  }

  "Local date" should {
    val DefaultReads = implicitly[Reads[LocalDate]]
    import DefaultReads.reads

    val CustomReads1 = Reads.localDateReads("dd/MM/yyyy")

    @inline def date(input: String) = LocalDate.parse(input)

    lazy val correctedReads = Reads.localDateReads(
      DateTimeFormatter.ISO_DATE,
      _.drop(1)
    )

    val CustomReads2 = Reads.localDateReads(
      DateTimeFormatter.ofPattern("dd/MM/yyyy"),
      _.drop(2)
    )

    "be successfully read from number" in {
      val beforeMidnight = Instant.parse("1970-01-01T23:55:00Z")
      val d              = LocalDate.parse("1970-01-01")

      reads(JsNumber(JBigDec.valueOf(beforeMidnight.toEpochMilli))).aka("read date").must_===(JsSuccess(d))
    }

    "not be read from invalid number" in {
      reads(JsNumber(BigDecimal("1000000000e1000000000"))).must(beLike { case JsError.Message("error.expected.long") =>
        ok
      })
    }

    "not be read from invalid string" in {
      reads(JsString("invalid")).must(beLike { case JsError.Message("error.expected.date.isoformat") =>
        ok
      })
    }

    "be successfully read with default implicit from '2011-12-03'" in {
      reads(JsString("2011-12-03")).must_===(JsSuccess(date("2011-12-03")))
    }

    "be successfully read with custom pattern from '03/12/2011'" in {
      CustomReads1.reads(JsString("03/12/2011")).aka("read date").must_===(JsSuccess(date("2011-12-03")))
    }

    "not be read from invalid corrected string" >> {
      "with default implicit" in {
        correctedReads
          .reads(JsString("2011-12-03"))
          .must(beLike { case JsError.Message("error.expected.date.isoformat") =>
            ok
          })
      }

      "with custom formatter" in {
        CustomReads2
          .reads(JsString("03/12/2011"))
          .must(beLike { case JsError.Message("error.expected.date.isoformat") =>
            ok
          })
      }
    }

    "be successfully read from corrected string" >> {
      lazy val d = date("2011-12-03")

      "with default implicit" in {
        correctedReads.reads(JsString("_2011-12-03")).aka("read date").must_===(JsSuccess(d))
      }

      "with custom formatter" in {
        CustomReads2.reads(JsString("# 03/12/2011")).must_===(JsSuccess(d))
      }
    }
  }

  "Local time" should {
    val DefaultReads = implicitly[Reads[LocalTime]]
    import DefaultReads.reads

    val CustomReads1 = Reads.localTimeReads("HH.mm.ss")

    lazy val correctedReads = Reads.localTimeReads(
      DateTimeFormatter.ISO_TIME,
      _.drop(1)
    )

    val CustomReads2 = Reads.localTimeReads(
      DateTimeFormatter.ofPattern("HH.mm.ss"),
      _.drop(2)
    )

    "be successfully read from number" in {
      val d = LocalTime.parse("10:15")

      reads(JsNumber(BigDecimal.valueOf(d.toNanoOfDay))).must_===(JsSuccess(d))
    }

    "not be read from invalid number" in {
      reads(JsNumber(BigDecimal("1000000000e1000000000"))).must(beLike { case JsError.Message("error.expected.long") =>
        ok
      })
    }

    "not be read from invalid string" in {
      reads(JsString("invalid")).must(beLike { case JsError.Message("error.expected.date.isoformat") =>
        ok
      })
    }

    "be successfully read with default implicit from '10:15:30'" in {
      reads(JsString("10:15:30")).must_===(JsSuccess(LocalTime.of(10, 15, 30)))
    }

    "be successfully read with custom pattern from '10.15.30'" in {
      CustomReads1.reads(JsString("10.15.30")).aka("read time").must_===(JsSuccess(LocalTime.of(10, 15, 30)))
    }

    "not be read from invalid corrected string" >> {
      "with default implicit" in {
        correctedReads
          .reads(JsString("0:15:30"))
          .must(beLike { case JsError.Message("error.expected.date.isoformat") =>
            ok
          })
      }

      "with custom formatter" in {
        CustomReads2
          .reads(JsString("10:15:30"))
          .must(beLike { case JsError.Message("error.expected.date.isoformat") =>
            ok
          })
      }
    }

    "be successfully read from corrected string" >> {
      lazy val d = LocalTime.of(10, 15, 30)

      "with default implicit" in {
        correctedReads.reads(JsString("_10:15:30")).must_===(JsSuccess(d))
      }

      "with custom formatter" in {
        CustomReads2.reads(JsString("# 10.15.30")).must_===(JsSuccess(d))
      }
    }
  }

  "Instant" should {
    val DefaultReads = implicitly[Reads[Instant]]
    import DefaultReads.reads

    val CustomReads1 = Reads.instantReads("dd/MM/yyyy, HH:mm:ss X")

    lazy val correctedReads = Reads.instantReads(
      DateTimeFormatter.ISO_DATE_TIME.withZone(ZoneOffset.UTC),
      _.drop(1)
    )

    val CustomReads2 = Reads.instantReads(
      DateTimeFormatter.ofPattern("dd/MM/yyyy, HH:mm:ss").withZone(ZoneOffset.UTC),
      _.drop(2)
    )

    "be successfully read from number" in {
      reads(JsNumber(JBigDec.valueOf(123L))).must_===(JsSuccess(Instant.ofEpochMilli(123L)))
    }

    "not be read from invalid number" in {
      reads(JsNumber(BigDecimal("1000000000e1000000000")))
        .aka("read date")
        .must(beLike { case JsError.Message("error.expected.long") =>
          ok
        })
    }

    "not be read from invalid string" in {
      reads(JsString("invalid")).must(beLike { case JsError.Message("error.expected.date.isoformat") =>
        ok
      })
    }

    "be successfully read with default implicit" >> {
      "from '2015-05-01T13:00:00Z' (with zeros)" in {
        reads(JsString("2015-05-01T00:00:00Z")).must_===(JsSuccess(Instant.parse("2015-05-01T00:00:00Z")))
      }

      "from '2011-12-03T10:15:30Z'" in {
        reads(JsString("2011-12-03T10:15:30Z")).must_===(JsSuccess(Instant.parse("2011-12-03T10:15:30Z")))
      }

      "from '2015-05-01T13:00:00+02:00' (with TZ offset and zeros)" in {
        reads(JsString("2015-05-01T13:00:00+02:00")).must_==(
          JsSuccess(
            Instant.parse("2015-05-01T11:00:00Z")
          )
        )
      }

      "from '2011-12-03T10:15:30+01:00' (with TZ offset)" in {
        reads(JsString("2011-12-03T10:15:30+01:00"))
          .aka("read date")
          .must_==(
            JsSuccess(Instant.parse("2011-12-03T09:15:30Z"))
          )
      }

      "from '2011-12-03T10:15:30+01:00[Europe/Paris]' (with time zone)" in {
        reads(JsString("2011-12-03T10:15:30+01:00[Europe/Paris]"))
          .aka("read date")
          .must_==(
            JsSuccess(Instant.parse("2011-12-03T09:15:30Z"))
          )
      }

      "from '2011-12-03T00:00:00+01:00[Europe/Paris]' (with time zone)" in {
        reads(JsString("2011-12-03T00:00:00+01:00[Europe/Paris]"))
          .aka("read date")
          .must_==(
            JsSuccess(Instant.parse("2011-12-02T23:00:00Z"))
          )
      }
    }

    "be successfully read with custom pattern from '03/12/2011, 10:15:30 Z'" in {
      CustomReads1
        .reads(JsString("03/12/2011, 10:15:30 Z"))
        .aka("read date")
        .must_==(JsSuccess(Instant.parse("2011-12-03T10:15:30Z")))
    }

    "not be read from invalid corrected string" >> {
      "with default implicit" in {
        correctedReads
          .reads(JsString("2011-12-03T10:15:30"))
          .must(beLike { case JsError.Message("error.expected.date.isoformat") =>
            ok
          })
      }

      "with custom formatter" in {
        CustomReads2
          .reads(JsString("03/12/2011, 10:15:30"))
          .must(beLike { case JsError.Message("error.expected.date.isoformat") =>
            ok
          })
      }
    }

    "be successfully read from corrected string" >> {
      lazy val time = Instant.parse("2011-12-03T10:15:30Z")

      "with default implicit" in {
        correctedReads.reads(JsString("_2011-12-03T10:15:30")).aka("read date").must_==(JsSuccess(time))
      }

      "with custom formatter" in {
        CustomReads2.reads(JsString("# 03/12/2011, 10:15:30")).aka("read date").must_==(JsSuccess(time))
      }
    }
  }

  "ZoneId" should {
    val DefaultReads = implicitly[Reads[ZoneId]]
    import DefaultReads.reads

    val validTimeZones = "America/Los_Angeles" :: "UTC" :: "CET" :: "UTC-8" :: Nil

    Fragment.foreach(validTimeZones)(tz =>
      s"be successfully read from $tz" in {
        reads(JsString(tz)).must_===(JsSuccess(ZoneId.of(tz)))
      }
    )

    "not be read from number" in {
      val reads1 = reads(JsNumber(123))
      reads1.must(beLike { case JsError.Message("error.expected.jsstring") =>
        ok
      })
    }

    "not be read from unknown time zone" in {
      reads(JsString("America/Gotham")).must(beLike {
        case JsError.Detailed("error.expected.timezone", "America/Gotham") => ok
      })
    }

    "not be read from malformed time zone" in {
      reads(JsString("UTC+x")).must(beLike { case JsError.Detailed("error.expected.timezone", "UTC+x") =>
        ok
      })
    }
  }

  "Locale" should {
    import LocaleFixtures._

    Fragment.foreach(locales.zip(objs)) { case (locale, obj) =>
      s"be ${locale.toLanguageTag} and be read as JSON object" in {
        Json.fromJson[Locale](obj)(Reads.localeObjectReads).mustEqual(JsSuccess(locale))
      }
    }

    Fragment.foreach(locales.zip(tags)) { case (locale, tag) =>
      s"be ${locale.toLanguageTag} and be read from JSON string (tag)" in {
        Json.fromJson[Locale](JsString(tag)).must_==(JsSuccess(locale))
      }
    }
  }

  "Java Duration" should {
    val oneSec = JDuration.of(1L, ChronoUnit.SECONDS)

    Fragment.foreach[(JsValue, JsResult[JDuration])](
      Seq(
        JsString("PT1S")             -> JsSuccess(oneSec),
        JsString("1 seconds")        -> JsError("error.invalid.duration"),
        JsString("foo")              -> JsError("error.invalid.duration"),
        JsNumber(BigDecimal(1000L))  -> JsSuccess(oneSec),
        JsNumber(BigDecimal(1.234D)) -> JsError("error.expected.long")
      )
    ) { case (input, result) =>
      s"be parsed from ${Json.stringify(input)} as $result" in {
        Json.fromJson[JDuration](input).mustEqual(result)
      }
    }
  }

  "Java Period" should {
    val twoDays = Period.ofDays(2)
    val period1 = Period.ofWeeks(3).minus(twoDays)
    val period2 = Period.ofMonths(2).plus(period1)

    Fragment.foreach[(JsValue, JsResult[Period])](
      Seq(
        JsString("P2D")              -> JsSuccess(twoDays),
        JsNumber(BigDecimal(2L))     -> JsSuccess(twoDays),
        JsString("2 days")           -> JsError("error.invalid.stringPeriod"),
        JsString("P2W")              -> JsSuccess(Period.ofWeeks(2)),
        JsString("P19D")             -> JsSuccess(period1),
        JsNumber(BigDecimal(19L))    -> JsSuccess(period1),
        JsString("P2M19D")           -> JsSuccess(period2),
        JsString("foo")              -> JsError("error.invalid.stringPeriod"),
        JsNumber(BigDecimal(1.234D)) -> JsError("error.expected.int")
      )
    ) { case (input, result) =>
      s"be parsed from ${Json.stringify(input)} as $result" in {
        Json.fromJson[Period](input).mustEqual(result)
      }
    }

    Fragment.foreach[(Long, Period)](
      Seq(
        2L  -> twoDays,
        19L -> period1
      )
    ) { case (days, result) =>
      s"be parsed as days from '$days' as $result" in {
        Json.fromJson[Period](JsNumber(days))(Reads.javaPeriodDaysReads).mustEqual(JsSuccess(result))
      }
    }

    "be parsed as weeks" in {
      Json.fromJson[Period](JsNumber(3))(Reads.javaPeriodWeeksReads).must_==(JsSuccess(Period.ofWeeks(3)))
    }

    "be parsed as months" in {
      Json.fromJson[Period](JsNumber(4))(Reads.javaPeriodMonthsReads).must_==(JsSuccess(Period.ofMonths(4)))
    }

    "be parsed as years" in {
      Json.fromJson[Period](JsNumber(5))(Reads.javaPeriodYearsReads).must_==(JsSuccess(Period.ofYears(5)))
    }
  }

  "Long numbers" should {
    val DefaultReads = implicitly[Reads[Long]]
    import DefaultReads.reads

    "parse a long number" in {
      reads(JsNumber(JBigDec.valueOf(123L))).aka("read long number").must_===(JsSuccess(123L))
    }

    "parse a negative long number" in {
      reads(JsNumber(JBigDec.valueOf(-123L))).aka("read long number").must_===(JsSuccess(-123L))
    }

    "not read from invalid number" in {
      reads(JsNumber(BigDecimal("1000000000e1000000000")))
        .aka("read long number")
        .must(beLike { case JsError.Message("error.expected.long") =>
          ok
        })
    }

    "parse a large long number 2" in {
      reads(JsNumber(veryLargeNumber))
        .aka("read long number")
        .must(beLike { case JsError.Message("error.expected.long") =>
          ok
        })
    }
  }

  "BigDecimal numbers" should {
    val DefaultReads = implicitly[Reads[BigDecimal]]
    import DefaultReads.reads

    val settings = JsonParserSettings.settings.bigDecimalParseSettings

    val longNumberString =
      Iterator
        .fill(settings.digitsLimit)("1")
        .mkString

    "parse long string" in {
      reads(JsString(longNumberString)) must_=== JsSuccess(
        BigDecimal(longNumberString, settings.mathContext)
      )
    }

    "not parse string exceeding digit limit" in {
      reads(JsString(longNumberString + "1")) must_=== JsError("error.expected.numberdigitlimit")
    }

    "parse string with acceptable scale" in {
      val numberString = s"1E+${settings.scaleLimit}"
      reads(JsString(numberString)) must_=== JsSuccess(BigDecimal(numberString))
    }

    "not parse string exceeding scale limit" in {
      val numberString = s"1E+${settings.scaleLimit + 1}"
      reads(JsString(numberString)) must_=== JsError(JsonValidationError("error.expected.numberscalelimit", -6179))
    }
  }

  "BigInt numbers" should {
    val DefaultReads = implicitly[Reads[BigInt]]
    import DefaultReads.reads

    val settings = JsonParserSettings.settings.bigDecimalParseSettings

    val longNumberString =
      Iterator
        .fill(settings.digitsLimit)("1")
        .mkString

    "parse long string" in {
      reads(JsString(longNumberString)) must_=== JsSuccess(BigInt(longNumberString))
    }

    "not parse string exceeding digit limit" in {
      reads(JsString(longNumberString + "1")) must_=== JsError("error.expected.numberdigitlimit")
    }
  }

  "Map" should {
    "be successfully read with custom (locale) keys" in {
      Json
        .obj("en" -> 1, "fr" -> 2)
        .validate[Map[Locale, Int]]
        .must_===(JsSuccess(Map(Locale.ENGLISH -> 1, Locale.FRENCH -> 2)))
    }
  }
}
