/*
 * Copyright (C) 2009-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import java.time.format.DateTimeFormatter
import java.time.temporal.Temporal
import java.time.Instant
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime
import java.time.OffsetDateTime
import java.time.Period
import java.time.ZoneId
import java.time.ZoneOffset
import java.time.ZonedDateTime
import java.time.{ Duration => JDuration }
import java.util.Locale

import com.fasterxml.jackson.databind.JsonNode
import play.api.libs.json.jackson.JacksonJson

trait EnvWrites {
  import scala.language.implicitConversions

  @deprecated("Use `jsonNodeWrites`", "2.8.0")
  object JsonNodeWrites extends Writes[JsonNode] {
    def writes(o: JsonNode): JsValue = JacksonJson.jsonNodeToJsValue(o)
  }

  /**
   * Serializer for Jackson JsonNode
   */
  implicit def jsonNodeWrites[T <: JsonNode]: Writes[T] =
    Writes[T](JacksonJson.jsonNodeToJsValue)

  /** Typeclass to implement way of formatting of Java8 temporal types. */
  trait TemporalFormatter[T <: Temporal] {
    def format(temporal: T): String
  }

  /** Formatting companion */
  object TemporalFormatter {
    implicit def DefaultLocalDateTimeFormatter(formatter: DateTimeFormatter): TemporalFormatter[LocalDateTime] =
      new TemporalFormatter[LocalDateTime] {
        def format(temporal: LocalDateTime): String = formatter.format(temporal)
      }

    implicit def PatternLocalDateTimeFormatter(pattern: String): TemporalFormatter[LocalDateTime] =
      DefaultLocalDateTimeFormatter(DateTimeFormatter.ofPattern(pattern))

    implicit def DefaultOffsetDateTimeFormatter(formatter: DateTimeFormatter): TemporalFormatter[OffsetDateTime] =
      new TemporalFormatter[OffsetDateTime] {
        def format(temporal: OffsetDateTime): String = formatter.format(temporal)
      }

    implicit def PatternOffsetDateTimeFormatter(pattern: String): TemporalFormatter[OffsetDateTime] =
      DefaultOffsetDateTimeFormatter(DateTimeFormatter.ofPattern(pattern))

    implicit def DefaultZonedDateTimeFormatter(formatter: DateTimeFormatter): TemporalFormatter[ZonedDateTime] =
      new TemporalFormatter[ZonedDateTime] {
        def format(temporal: ZonedDateTime): String = formatter.format(temporal)
      }

    implicit def PatternZonedDateTimeFormatter(pattern: String): TemporalFormatter[ZonedDateTime] =
      DefaultZonedDateTimeFormatter(DateTimeFormatter.ofPattern(pattern))

    implicit def DefaultDateFormatter(formatter: DateTimeFormatter): TemporalFormatter[LocalDate] =
      new TemporalFormatter[LocalDate] {
        def format(temporal: LocalDate): String = formatter.format(temporal)
      }

    implicit def PatternDateFormatter(pattern: String): TemporalFormatter[LocalDate] =
      DefaultDateFormatter(DateTimeFormatter.ofPattern(pattern))

    implicit def DefaultInstantFormatter(formatter: DateTimeFormatter): TemporalFormatter[Instant] =
      new TemporalFormatter[Instant] {
        def format(temporal: Instant): String = formatter.format(temporal)
      }

    implicit def PatternInstantFormatter(pattern: String): TemporalFormatter[Instant] =
      DefaultInstantFormatter(DateTimeFormatter.ofPattern(pattern).withZone(ZoneOffset.UTC))

    implicit def DefaultLocalTimeFormatter(formatter: DateTimeFormatter): TemporalFormatter[LocalTime] =
      new TemporalFormatter[LocalTime] {
        def format(temporal: LocalTime): String = formatter.format(temporal)
      }

    implicit def PatternLocalTimeFormatter(pattern: String): TemporalFormatter[LocalTime] =
      DefaultLocalTimeFormatter(DateTimeFormatter.ofPattern(pattern).withZone(ZoneOffset.UTC))
  }

  /**
   * Serializer for Java8 temporal types (e.g. `java.time.LocalDateTime`)
   * to be written as JSON string.
   *
   * @tparam A the Java8 temporal type to be considered: LocalDateTime, ZonedDateTime, Instant
   * @tparam B Type of formatting argument
   *
   * @param formatting an argument to instantiate formatter
   *
   * {{{
   * import java.time.LocalDateTime
   * import java.time.format.DateTimeFormatter
   *
   * import play.api.libs.json.Writes
   *
   * implicit val w: Writes[LocalDateTime] =
   *   Writes.temporalWrites[LocalDateTime, DateTimeFormatter](
   *     DateTimeFormatter.ISO_LOCAL_DATE_TIME)
   * }}}
   */
  def temporalWrites[A <: Temporal, B](formatting: B)(implicit f: B => TemporalFormatter[A]): Writes[A] =
    new Writes[A] {
      def writes(temporal: A): JsValue = JsString(f(formatting).format(temporal))
    }

  /**
   * The default typeclass to write a `java.time.LocalDateTime`,
   * using '2011-12-03T10:15:30' format.
   */
  implicit val DefaultLocalDateTimeWrites: Writes[LocalDateTime] =
    temporalWrites[LocalDateTime, DateTimeFormatter](
      DateTimeFormatter.ISO_LOCAL_DATE_TIME
    )

  /**
   * The default typeclass to write a `java.time.OffsetDateTime`,
   * using '2011-12-03T10:15:30+02:00' format.
   */
  implicit val DefaultOffsetDateTimeWrites: Writes[OffsetDateTime] =
    temporalWrites[OffsetDateTime, DateTimeFormatter](
      DateTimeFormatter.ISO_OFFSET_DATE_TIME
    )

  /**
   * The default typeclass to write a `java.time.ZonedDateTime`,
   * using '2011-12-03T10:15:30+01:00[Europe/Paris]' format.
   */
  implicit val DefaultZonedDateTimeWrites: Writes[ZonedDateTime] =
    temporalWrites[ZonedDateTime, DateTimeFormatter](
      DateTimeFormatter.ISO_ZONED_DATE_TIME
    )

  /**
   * The default typeclass to write a `java.time.LocalDate`,
   * using '2011-12-03' format.
   */
  implicit val DefaultLocalDateWrites: Writes[LocalDate] =
    temporalWrites[LocalDate, DateTimeFormatter](
      DateTimeFormatter.ISO_LOCAL_DATE
    )

  /**
   * The default typeclass to write a `java.time.Instant`,
   * using '2011-12-03T10:15:30Z' format.
   */
  implicit val DefaultInstantWrites: Writes[Instant] =
    Writes[Instant] { i =>
      JsString(i.toString)
    }

  /**
   * The default typeclass to write a `java.time.LocalTime`,
   * using '10:15:30' format.
   */
  implicit val DefaultLocalTimeWrites: Writes[LocalTime] =
    temporalWrites[LocalTime, DateTimeFormatter](DateTimeFormatter.ISO_TIME)

  /**
   * Serializer for `java.time.LocalTime` as JSON number.
   * The nano of day is written.
   *
   * {{{
   * import java.time.LocalTime
   * import play.api.libs.json.Writes
   *
   * implicit val ltnWrites = Writes.LocalTimeNanoOfDayWrites
   * }}}
   */
  val LocalTimeNanoOfDayWrites: Writes[LocalTime] = Writes[LocalTime] { t =>
    JsNumber(BigDecimal.valueOf(t.toNanoOfDay))
  }

  /**
   * Serializer for `java.time.ZoneId` as JSON string.
   */
  implicit val ZoneIdWrites: Writes[ZoneId] =
    Writes[ZoneId](zone => JsString(zone.getId))

  /**
   * Serializer for `java.time.LocalDateTime` as JSON number.
   * The number of milliseconds since epoch is used.
   *
   * {{{
   * import java.time.LocalDateTime
   * import play.api.libs.json.Writes
   *
   * implicit val ldtnWrites = Writes.LocalDateTimeEpochMilliWrites
   * }}}
   */
  val LocalDateTimeEpochMilliWrites: Writes[LocalDateTime] =
    Writes[LocalDateTime] { t =>
      JsNumber(
        BigDecimal.valueOf(
          t.toInstant(ZoneOffset.UTC).toEpochMilli
        )
      )
    }

  /**
   * Serializer for `java.time.ZonedDateTime` as JSON number.
   * The number of milliseconds since epoch is used.
   *
   * {{{
   * import java.time.ZonedDateTime
   * import play.api.libs.json.Writes
   *
   * implicit val zdtnWrites = Writes.ZonedDateTimeEpochMilliWrites
   * }}}
   */
  val ZonedDateTimeEpochMilliWrites: Writes[ZonedDateTime] =
    new Writes[ZonedDateTime] {
      def writes(t: ZonedDateTime): JsValue =
        JsNumber(BigDecimal.valueOf(t.toInstant.toEpochMilli))
    }

  /**
   * Serializer for `java.time.LocalDate` as JSON number.
   * The number of milliseconds since epoch is used.
   *
   * {{{
   * import java.time.LocalDate
   * import play.api.libs.json.Writes
   *
   * implicit val ldnWrites: Writes[LocalDate] =
   *   Writes.LocalDateEpochMilliWrites
   * }}}
   */
  val LocalDateEpochMilliWrites: Writes[LocalDate] = Writes[LocalDate] { t =>
    JsNumber(
      BigDecimal.valueOf(
        t.atStartOfDay.toInstant(ZoneOffset.UTC).toEpochMilli
      )
    )
  }

  /**
   * Serializer for `java.time.Instant` as JSON number.
   * The number of milliseconds since epoch is used.
   *
   * {{{
   * import java.time.Instant
   * import play.api.libs.json.Writes
   *
   * implicit val inWrites: Writes[Instant] = Writes.InstantEpochMilliWrites
   * }}}
   */
  val InstantEpochMilliWrites: Writes[Instant] = new Writes[Instant] {
    def writes(t: Instant): JsValue =
      JsNumber(BigDecimal.valueOf(t.toEpochMilli))
  }

  /** Serializer for a `Locale` using the IETF BCP 47 string representation */
  implicit val localeWrites: Writes[Locale] =
    Writes[Locale] { l =>
      JsString(KeyWrites.LanguageTagWrites.writeKey(l))
    }

  /** Serializer for a `Locale` using a object representation */
  val localeObjectWrites: OWrites[Locale] = {
    import scala.collection.JavaConverters.asScalaSetConverter

    OWrites[Locale] { l =>
      val fields = Map.newBuilder[String, JsValue]

      fields += "language" -> Json.toJson(l.getLanguage)

      Option(l.getCountry).filter(_.nonEmpty).foreach { country =>
        fields += "country" -> Json.toJson(country)
      }

      Option(l.getVariant).filter(_.nonEmpty).foreach { variant =>
        fields += "variant" -> Json.toJson(variant)
      }

      Option(l.getScript).filter(_.nonEmpty).foreach { script =>
        fields += "script" -> Json.toJson(script)
      }

      val attrs = l.getUnicodeLocaleAttributes.asScala
      if (attrs.nonEmpty) {
        fields += "attributes" -> Json.toJson(attrs.toSet)
      }

      val keywords = l.getUnicodeLocaleKeys.asScala
      if (keywords.nonEmpty) {
        fields += "keywords" -> Json.toJson {
          val ks = Map.newBuilder[String, String]

          keywords.foreach { key =>
            Option(l.getUnicodeLocaleType(key)).foreach { typ =>
              ks += (key -> typ)
            }
          }

          ks.result()
        }
      }

      val extension = l.getExtensionKeys.asScala
      if (extension.nonEmpty) {
        fields += "extension" -> Json.toJson {
          val ext = Map.newBuilder[String, String]

          extension.foreach { key =>
            Option(l.getExtension(key)).foreach { v =>
              ext += (key.toString -> v)
            }
          }

          ext.result()
        }
      }

      JsObject(fields.result())
    }
  }

  /** Serializer of Java Duration as a number of milliseconds. */
  val javaDurationMillisWrites: Writes[JDuration] =
    Writes[JDuration] { d =>
      JsNumber(d.toMillis)
    }

  /**
   * Serializer of Java Duration using ISO representation
   * (e.g. PT1S for 1 second).
   */
  implicit val javaDurationWrites: Writes[JDuration] =
    Writes[JDuration] { d =>
      JsString(d.toString)
    }

  /**
   * Serializer of Java Period using ISO representation
   * (e.g. P2D for 2 days).
   */
  implicit val javaPeriodWrites: Writes[Period] =
    Writes[Period] { d =>
      JsString(d.toString)
    }
}

trait EnvKeyWrites {
  implicit object LanguageTagWrites extends KeyWrites[Locale] {
    def writeKey(locale: Locale): String = locale.toLanguageTag
  }
}
