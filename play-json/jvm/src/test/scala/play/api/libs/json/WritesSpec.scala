/*
 * Copyright (C) 2009-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import java.util.Locale

import java.time.Instant
import java.time.{ Duration => JDuration }
import java.time.Period
import java.time.LocalDateTime
import java.time.LocalDate
import java.time.LocalTime
import java.time.OffsetDateTime
import java.time.ZonedDateTime
import java.time.ZoneOffset
import java.time.ZoneId
import java.time.temporal.ChronoUnit

import org.specs2.specification.core.Fragment

class WritesSpec extends org.specs2.mutable.Specification {
  title("JSON Writes")

  "Local date/time" should {
    val DefaultWrites = implicitly[Writes[LocalDateTime]]
    import DefaultWrites.writes

    @inline def dateTime(input: String) = LocalDateTime.parse(input)

    val CustomWrites1 = Writes.temporalWrites[LocalDateTime, String]("dd/MM/yyyy, HH:mm:ss")

    "be written as number" in {
      Writes.LocalDateTimeEpochMilliWrites
        .writes(
          LocalDateTime.ofInstant(
            Instant.ofEpochMilli(1234567890L),
            ZoneOffset.UTC
          )
        )
        .aka("written date")
        .must_==(JsNumber(BigDecimal.valueOf(1234567890L)))
    }

    "be written with default implicit as '2011-12-03T10:15:30'" in {
      writes(dateTime("2011-12-03T10:15:30"))
        .aka("written date")
        .must_==(
          JsString("2011-12-03T10:15:30")
        )
    }

    "be written with custom pattern as '03/12/2011, 10:15:30'" in {
      CustomWrites1
        .writes(dateTime("2011-12-03T10:15:30"))
        .aka("written date")
        .must_==(JsString("03/12/2011, 10:15:30"))
    }
  }

  "Offset date/time" should {
    val DefaultWrites = implicitly[Writes[OffsetDateTime]]
    import DefaultWrites.writes

    val CustomWrites1 = Writes.temporalWrites[OffsetDateTime, String]("dd/MM/yyyy, HH:mm:ss (XXX)")

    "be written with default implicit as '2011-12-03T10:15:30-01:30'" in {
      writes(OffsetDateTime.parse("2011-12-03T10:15:30-01:30"))
        .aka("written date")
        .must_==(
          JsString("2011-12-03T10:15:30-01:30")
        )
    }

    "be written with custom pattern as '03/12/2011, 10:15:30 (-01:30)'" in {
      CustomWrites1
        .writes(OffsetDateTime.parse("2011-12-03T10:15:30-01:30"))
        .aka("written date")
        .must_==(JsString("03/12/2011, 10:15:30 (-01:30)"))
    }
  }

  "Zoned date/time" should {
    val DefaultWrites = implicitly[Writes[ZonedDateTime]]
    import DefaultWrites.writes

    @inline def dateTime(input: String) = ZonedDateTime.parse(input)

    val CustomWrites1 = Writes.temporalWrites[ZonedDateTime, String]("dd/MM/yyyy, HH:mm:ss")

    "be written as number" in {
      Writes.ZonedDateTimeEpochMilliWrites
        .writes(
          ZonedDateTime.ofInstant(
            Instant.ofEpochMilli(1234567890L),
            ZoneOffset.UTC
          )
        )
        .aka("written date")
        .must_==(JsNumber(BigDecimal.valueOf(1234567890L)))
    }

    "be written with default implicit as '2011-12-03T10:15:30+01:00[Europe/Paris]'" in {
      writes(dateTime("2011-12-03T10:15:30+01:00[Europe/Paris]"))
        .aka("written date")
        .must_==(
          JsString("2011-12-03T10:15:30+01:00[Europe/Paris]")
        )
    }

    "be written with default implicit as '2011-12-03T10:15:30+06:30'" in {
      writes(dateTime("2011-12-03T10:15:30+06:30"))
        .aka("written date")
        .must_==(
          JsString("2011-12-03T10:15:30+06:30")
        )
    }

    "be written with custom pattern as '03/12/2011, 10:15:30'" in {
      CustomWrites1
        .writes(dateTime("2011-12-03T10:15:30+05:30"))
        .aka("written date")
        .must_==(JsString("03/12/2011, 10:15:30"))
    }
  }

  "Local date" should {
    val DefaultWrites = implicitly[Writes[LocalDate]]
    import DefaultWrites.writes

    @inline def date(input: String) = LocalDate.parse(input)

    val CustomWrites1 = Writes.temporalWrites[LocalDate, String]("dd/MM/yyyy")

    "be written as number" in {
      Writes.LocalDateEpochMilliWrites
        .writes(
          LocalDate.ofEpochDay(1234567890L)
        )
        .aka("written date")
        .must_==(
          JsNumber(
            BigDecimal.valueOf(106666665696000000L)
          )
        )
    }

    "be written with default implicit as '2011-12-03'" in {
      writes(date("2011-12-03"))
        .aka("written date")
        .must_==(
          JsString(
            "2011-12-03"
          )
        )
    }

    "be written with custom pattern as '03/12/2011'" in {
      CustomWrites1.writes(date("2011-12-03")).aka("written date").must_==(JsString("03/12/2011"))
    }
  }

  "Local time" should {
    val DefaultWrites = implicitly[Writes[LocalTime]]
    import DefaultWrites.writes

    val CustomWrites1 = Writes.temporalWrites[LocalTime, String]("HH.mm.ss")

    "be written as number" in {
      Writes.LocalTimeNanoOfDayWrites
        .writes(
          LocalTime.ofNanoOfDay(1234567890L)
        )
        .aka("written time")
        .must_==(JsNumber(BigDecimal.valueOf(1234567890L)))
    }

    "be written with default implicit as '10:15:30'" in {
      writes(LocalTime.of(10, 15, 30)).must_==(JsString("10:15:30"))
    }

    "be written with custom pattern as '10.15.30'" in {
      CustomWrites1.writes(LocalTime.of(10, 15, 30)).must_==(JsString("10.15.30"))
    }
  }

  "Instant" should {
    val DefaultWrites = implicitly[Writes[Instant]]
    import DefaultWrites.writes

    lazy val instant = Instant.parse("2011-12-03T10:15:30Z")

    val customPattern1 = "dd/MM/yyyy, HH:mm:ss"
    val CustomWrites1  = Writes.temporalWrites[Instant, String](customPattern1)

    "be written as number" in {
      Writes.InstantEpochMilliWrites
        .writes(Instant.ofEpochMilli(1234567890L))
        .aka("written date")
        .must_==(JsNumber(BigDecimal.valueOf(1234567890L)))
    }

    "be written with default implicit as '2011-12-03T10:15:30Z'" in {
      writes(instant).aka("written date").must_==(JsString("2011-12-03T10:15:30Z"))
    }

    "be written with custom pattern as '03/12/2011, 10:15:30'" in {
      CustomWrites1.writes(instant).aka("written date").must_==(JsString("03/12/2011, 10:15:30"))
    }
  }

  "ZoneId" should {
    val DefaultWrites = implicitly[Writes[ZoneId]]
    import DefaultWrites.writes

    val validTimeZones = "America/Los_Angeles" :: "UTC" :: "CET" :: "UTC-8" :: Nil

    Fragment.foreach(validTimeZones)(tz =>
      s"be written as time zone string $tz" in {
        val zoneId = ZoneId.of(tz)
        writes(zoneId).must_==(JsString(zoneId.getId))
      }
    )
  }

  "OWrites" should {
    val writes = OWrites[Foo] { foo =>
      Json.obj("bar" -> foo.bar)
    }
    val time = System.currentTimeMillis()

    "be transformed with JsObject function" in {
      val transformed: OWrites[Foo] = writes.transform((obj: JsObject) => obj ++ Json.obj("time" -> time))
      val written: JsObject         = transformed.writes(Foo("Lorem"))

      written.must_==(Json.obj("bar" -> "Lorem", "time" -> time))
    }

    "be transformed with a value transformation" in {
      val transformed: Writes[Foo] = Writes.transform(writes) {
        case (foo, obj @ JsObject(_)) =>
          obj ++ Json.obj("hash" -> foo.hashCode)

        case (_, v) => v
      }
      val foo              = Foo("Lorem")
      val written: JsValue = transformed.writes(foo)

      written.must_==(Json.obj("bar" -> "Lorem", "hash" -> foo.hashCode))
    }

    "be transformed with an object transformation" in {
      val transformed: OWrites[Foo] = OWrites.transform(writes) { (foo, obj) =>
        obj ++ Json.obj("hash" -> foo.hashCode)
      }
      val foo               = Foo("Lorem")
      val written: JsObject = transformed.writes(foo)

      written.must_==(Json.obj("bar" -> "Lorem", "hash" -> foo.hashCode))
    }

    "be transformed with another OWrites" in {
      val transformed: OWrites[Foo] =
        writes.transform(OWrites[JsObject] { obj =>
          obj ++ Json.obj("time" -> time)
        })
      val written: JsObject = transformed.writes(Foo("Lorem"))

      written.must_==(Json.obj("bar" -> "Lorem", "time" -> time))
    }
  }

  "Locale" should {
    import LocaleFixtures._

    Fragment.foreach(locales.zip(objs)) { case (locale, obj) =>
      s"be ${locale.toLanguageTag} and be written as JSON object" in {
        Json.toJson(locale)(Writes.localeObjectWrites).must_==(obj)
      }
    }

    Fragment.foreach(locales.zip(tags)) { case (locale, tag) =>
      s"be ${locale.toLanguageTag} and be written as JSON string (tag)" in {
        Json.toJson(locale).must_==(JsString(tag))
      }
    }
  }

  "Java Duration" should {
    "be written as milliseconds" in {
      Json
        .toJson(JDuration.of(1L, ChronoUnit.SECONDS))(Writes.javaDurationMillisWrites)
        .mustEqual(JsNumber(BigDecimal(1000L)))
    }

    "be written as ISO string" in {
      Json.toJson(JDuration.of(2L, ChronoUnit.DAYS)).mustEqual(JsString("PT48H"))
    }
  }

  "Java Period" should {
    val twoDays = Period.ofDays(2)
    val period1 = Period.ofWeeks(3).minus(twoDays)
    val period2 = Period.ofMonths(4).plus(period1)

    Fragment.foreach[(Period, String)](
      Seq(
        twoDays -> "P2D",
        period1 -> "P19D",
        period2 -> "P4M19D"
      )
    ) { case (period, repr) =>
      s"be written as ISO string '$repr'" in {
        Json.toJson(period).mustEqual(JsString(repr))
      }
    }
  }

  "Map" should {
    "be successfully written with custom (locale) keys" in {
      Json
        .toJson(
          Map(Locale.ENGLISH -> 1, Locale.FRENCH -> 2)
        )
        .must_==(Json.obj("en" -> 1, "fr" -> 2))
    }
  }

  case class Foo(bar: String)
}
