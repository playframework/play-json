/*
 * Copyright (C) 2009-2016 Lightbend Inc. <https://www.lightbend.com>
 */
package play.api.libs.json

import java.time.format.DateTimeFormatter
import java.time.temporal.Temporal
import java.time.{
  Instant,
  LocalDate,
  LocalDateTime,
  OffsetDateTime,
  ZoneOffset,
  ZonedDateTime,
  ZoneId
}

import com.fasterxml.jackson.databind.JsonNode

import play.api.libs.json.jackson.JacksonJson

trait EnvWrites {
  import scala.language.implicitConversions

  /**
   * Serializer for Jackson JsonNode
   */
  implicit object JsonNodeWrites extends Writes[JsonNode] {
    def writes(o: JsonNode): JsValue = JacksonJson.jsonNodeToJsValue(o)
  }

  /** Typeclass to implement way of formatting of Java8 temporal types. */
  trait TemporalFormatter[T <: Temporal] {
    def format(temporal: T): String
  }

  /** Formatting companion */
  object TemporalFormatter {
    implicit def DefaultLocalDateTimeFormatter(formatter: DateTimeFormatter): TemporalFormatter[LocalDateTime] = new TemporalFormatter[LocalDateTime] {
      def format(temporal: LocalDateTime): String = formatter.format(temporal)
    }

    implicit def PatternLocalDateTimeFormatter(pattern: String): TemporalFormatter[LocalDateTime] =
      DefaultLocalDateTimeFormatter(DateTimeFormatter.ofPattern(pattern))

    implicit def DefaultOffsetDateTimeFormatter(formatter: DateTimeFormatter): TemporalFormatter[OffsetDateTime] = new TemporalFormatter[OffsetDateTime] {
      def format(temporal: OffsetDateTime): String = formatter.format(temporal)
    }

    implicit def PatternOffsetDateTimeFormatter(pattern: String): TemporalFormatter[OffsetDateTime] =
      DefaultOffsetDateTimeFormatter(DateTimeFormatter.ofPattern(pattern))

    implicit def DefaultZonedDateTimeFormatter(formatter: DateTimeFormatter): TemporalFormatter[ZonedDateTime] = new TemporalFormatter[ZonedDateTime] {
      def format(temporal: ZonedDateTime): String = formatter.format(temporal)
    }

    implicit def PatternZonedDateTimeFormatter(pattern: String): TemporalFormatter[ZonedDateTime] =
      DefaultZonedDateTimeFormatter(DateTimeFormatter.ofPattern(pattern))

    implicit def DefaultDateFormatter(formatter: DateTimeFormatter): TemporalFormatter[LocalDate] = new TemporalFormatter[LocalDate] {
      def format(temporal: LocalDate): String = formatter.format(temporal)
    }

    implicit def PatternDateFormatter(pattern: String): TemporalFormatter[LocalDate] =
      DefaultDateFormatter(DateTimeFormatter.ofPattern(pattern))

    implicit def DefaultInstantFormatter(formatter: DateTimeFormatter): TemporalFormatter[Instant] = new TemporalFormatter[Instant] {
      def format(temporal: Instant): String = formatter.format(temporal)
    }

    implicit def PatternInstantFormatter(pattern: String): TemporalFormatter[Instant] =
      DefaultInstantFormatter(DateTimeFormatter.ofPattern(pattern).withZone(ZoneOffset.UTC))
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
   * import play.api.libs.json.Writes
   *
   * implicit val temporalWrites: Writes[LocalDateTime] =
   *   temporalWrites[LocalDateTime, DateTimeFormatter](
   *     DateTimeFormatter.ISO_LOCAL_DATE_TIME)
   * }}}
   */
  def temporalWrites[A <: Temporal, B](formatting: B)(implicit f: B => TemporalFormatter[A]): Writes[A] = new Writes[A] {
    def writes(temporal: A): JsValue = JsString(f(formatting) format temporal)
  }

  /**
   * The default typeclass to write a `java.time.LocalDateTime`,
   * using '2011-12-03T10:15:30' format.
   */
  implicit val DefaultLocalDateTimeWrites =
    temporalWrites[LocalDateTime, DateTimeFormatter](
      DateTimeFormatter.ISO_LOCAL_DATE_TIME
    )

  /**
   * The default typeclass to write a `java.time.OffsetDateTime`,
   * using '2011-12-03T10:15:30+02:00' format.
   */
  implicit val DefaultOffsetDateTimeWrites =
    temporalWrites[OffsetDateTime, DateTimeFormatter](
      DateTimeFormatter.ISO_OFFSET_DATE_TIME
    )

  /**
   * The default typeclass to write a `java.time.ZonedDateTime`,
   * using '2011-12-03T10:15:30+01:00[Europe/Paris]' format.
   */
  implicit val DefaultZonedDateTimeWrites =
    temporalWrites[ZonedDateTime, DateTimeFormatter](
      DateTimeFormatter.ISO_ZONED_DATE_TIME
    )

  /**
   * The default typeclass to write a `java.time.LocalDate`,
   * using '2011-12-03' format.
   */
  implicit val DefaultLocalDateWrites =
    temporalWrites[LocalDate, DateTimeFormatter](
      DateTimeFormatter.ISO_LOCAL_DATE
    )

  /**
   * The default typeclass to write a `java.time.Instant`,
   * using '2011-12-03T10:15:30Z' format.
   */
  implicit val DefaultInstantWrites = new Writes[Instant] {
    def writes(i: Instant): JsValue = JsString(i.toString)
  }

  /**
   * Serializer for `java.time.ZoneId` as JSON string.
   */
  implicit val ZoneIdWrites: Writes[ZoneId] = Writes[ZoneId](zone => JsString(zone.getId))

  /**
   * Serializer for `java.time.LocalDateTime` as JSON number.
   *
   * {{{
   * import java.time.LocalDateTime
   * import play.api.libs.json.Writes
   *
   * implicit val ldtnWrites = Writes.LocalDateTimeNumberWrites
   * }}}
   */
  val LocalDateTimeNumberWrites: Writes[LocalDateTime] =
    new Writes[LocalDateTime] {
      def writes(t: LocalDateTime): JsValue = JsNumber(BigDecimal.valueOf(
        t.toInstant(ZoneOffset.UTC).toEpochMilli
      ))
    }

  /**
   * Serializer for `java.time.ZonedDateTime` as JSON number.
   *
   * {{{
   * import java.time.ZonedDateTime
   * import play.api.libs.json.Writes
   *
   * implicit val zdtnWrites = Writes.ZonedDateTimeNumberWrites
   * }}}
   */
  val ZonedDateTimeNumberWrites: Writes[ZonedDateTime] =
    new Writes[ZonedDateTime] {
      def writes(t: ZonedDateTime): JsValue =
        JsNumber(BigDecimal valueOf t.toInstant.toEpochMilli)
    }

  /**
   * Serializer for `java.time.LocalDate` as JSON number.
   *
   * {{{
   * import java.time.LocalDate
   * import play.api.libs.json.Writes
   *
   * implicit val ldnWrites = Writes.LocalDateNumberWrites
   * }}}
   */
  val LocalDateNumberWrites: Writes[LocalDate] = new Writes[LocalDate] {
    def writes(t: LocalDate): JsValue = JsNumber(BigDecimal.valueOf(
      t.atStartOfDay.toInstant(ZoneOffset.UTC).toEpochMilli
    ))
  }

  /**
   * Serializer for `java.time.Instant` as JSON number.
   *
   * {{{
   * import java.time.Instant
   * import play.api.libs.json.Writes
   *
   * implicit val inWrites = Writes.InstantNumberWrites
   * }}}
   */
  val InstantNumberWrites: Writes[Instant] = new Writes[Instant] {
    def writes(t: Instant): JsValue =
      JsNumber(BigDecimal valueOf t.toEpochMilli)
  }

  // TODO: Move to a separate module + deprecation
  import org.joda.time.{ DateTime, LocalDate, LocalTime }

  /**
   * Serializer for DateTime
   * @param pattern the pattern used by SimpleDateFormat
   */
  def jodaDateWrites(pattern: String): Writes[DateTime] = new Writes[DateTime] {
    val df = org.joda.time.format.DateTimeFormat.forPattern(pattern)
    def writes(d: DateTime): JsValue = JsString(d.toString(df))
  }

  /**
   * Default Serializer DateTime -> JsNumber(d.getMillis (nb of ms))
   */
  implicit object DefaultJodaDateWrites extends Writes[DateTime] {
    def writes(d: DateTime): JsValue = JsNumber(d.getMillis)
  }

  /**
   * Serializer for LocalDate
   * @param pattern the pattern used by org.joda.time.format.DateTimeFormat
   */
  def jodaLocalDateWrites(pattern: String): Writes[LocalDate] = {
    val df = org.joda.time.format.DateTimeFormat.forPattern(pattern)
    Writes[LocalDate] { d => JsString(d.toString(df)) }
  }

  /**
   * Default Serializer LocalDate -> JsString(ISO8601 format (yyyy-MM-dd))
   */
  implicit object DefaultJodaLocalDateWrites extends Writes[LocalDate] {
    def writes(d: LocalDate): JsValue = JsString(d.toString)
  }

  /**
   * Serializer for LocalTime
   * @param pattern the pattern used by org.joda.time.format.DateTimeFormat
   */
  def jodaLocalTimeWrites(pattern: String): Writes[LocalTime] =
    Writes[LocalTime] { d => JsString(d.toString(pattern)) }

  /**
   * Default Serializer LocalDate -> JsString(ISO8601 format (HH:mm:ss.SSS))
   */
  implicit object DefaultJodaLocalTimeWrites extends Writes[LocalTime] {
    def writes(d: LocalTime): JsValue = JsString(d.toString)
  }
  // _Joda
}
