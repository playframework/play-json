/*
 * Copyright (C) 2009-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import java.time.format.DateTimeFormatter
import java.time.format.DateTimeParseException
import java.time.temporal.ChronoUnit
import java.time.temporal.TemporalUnit
import java.time.temporal.UnsupportedTemporalTypeException
import java.time.temporal.{ Temporal => JTemporal }
import java.time.Clock
import java.time.DateTimeException
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

import scala.util.control.NonFatal

import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.node.ArrayNode
import com.fasterxml.jackson.databind.node.ObjectNode

import play.api.libs.json.jackson.JacksonJson

trait EnvReads {
  import scala.language.implicitConversions

  /**
   * Deserializer for Jackson JsonNode
   */
  implicit object JsonNodeReads extends Reads[JsonNode] {

    def reads(json: JsValue): JsResult[JsonNode] =
      JsSuccess(JacksonJson.jsValueToJsonNode(json))
  }

  /**
   * Deserializer for Jackson ObjectNode
   */
  implicit object ObjectNodeReads extends Reads[ObjectNode] {

    def reads(json: JsValue): JsResult[ObjectNode] = {
      json.validate[JsObject].map(jo => JacksonJson.jsValueToJsonNode(jo).asInstanceOf[ObjectNode])
    }
  }

  /**
   * Deserializer for Jackson ArrayNode
   */
  implicit object ArrayNodeReads extends Reads[ArrayNode] {

    def reads(json: JsValue): JsResult[ArrayNode] = {
      json.validate[JsArray].map(ja => JacksonJson.jsValueToJsonNode(ja).asInstanceOf[ArrayNode])
    }
  }

  /**
   * Reads for the `java.util.Date` type.
   *
   * @param pattern a date pattern, as specified in `java.text.SimpleDateFormat`.
   * @param corrector a simple string transformation function that can be used to transform input String before parsing. Useful when standards are not exactly respected and require a few tweaks
   */
  def dateReads(pattern: String, corrector: String => String = identity): Reads[java.util.Date] =
    new Reads[java.util.Date] {

      def reads(json: JsValue): JsResult[java.util.Date] = json match {
        case n: JsNumber => n.validate[Long].map(l => new java.util.Date(l))
        case JsString(s) =>
          parseJDate(pattern, corrector(s)) match {
            case Some(d) => JsSuccess(d)
            case None =>
              JsError(
                Seq(
                  JsPath ->
                    Seq(JsonValidationError("error.expected.date.isoformat", pattern))
                )
              )
          }
        case _ =>
          JsError(
            Seq(
              JsPath ->
                Seq(JsonValidationError("error.expected.date"))
            )
          )
      }
    }

  private def parseJDate(pattern: String, input: String): Option[java.util.Date] = {
    // REMEMBER THAT SIMPLEDATEFORMAT IS NOT THREADSAFE
    val df = new java.text.SimpleDateFormat(pattern)
    df.setLenient(false)
    try {
      Some(df.parse(input))
    } catch {
      case x: java.text.ParseException =>
        None
    }
  }

  /**
   * the default implicit java.util.Date reads
   */
  implicit val DefaultDateReads: Reads[java.util.Date] = dateReads("yyyy-MM-dd")

  /**
   * ISO 8601 Reads
   */
  object IsoDateReads extends Reads[java.util.Date] {
    import java.util.Date

    val millisAndTz = "yyyy-MM-dd'T'HH:mm:ss.SSSX"
    val millis      = "yyyy-MM-dd'T'HH:mm:ss.SSS"
    val tz          = "yyyy-MM-dd'T'HH:mm:ssX"
    val mini        = "yyyy-MM-dd'T'HH:mm:ss"

    val WithMillisAndTz = """^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}\.[0-9]{3}.+$""".r

    val WithMillis = """^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}\.[0-9]{3}$""".r

    val WithTz = """^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}[^.]+$""".r

    def reads(json: JsValue): JsResult[Date] = json match {
      case n: JsNumber => n.validate[Long].map(l => new Date(l))

      case JsString(s) =>
        (s match {
          case WithMillisAndTz() => millisAndTz -> parseJDate(millisAndTz, s)
          case WithMillis()      => millis      -> parseJDate(millis, s)
          case WithTz()          => tz          -> parseJDate(tz, s)
          case _                 => mini        -> parseJDate(mini, s)
        }) match {
          case (_, Some(d)) => JsSuccess(d)
          case (p, None) =>
            JsError(
              Seq(
                JsPath ->
                  Seq(JsonValidationError("error.expected.date.isoformat", p))
              )
            )
        }

      case js => JsError("error.expected.date.isoformat")
    }
  }

  /**
   * Reads for the `java.sql.Date` type.
   *
   * @param pattern a date pattern, as specified in `java.text.SimpleDateFormat`.
   * @param corrector a simple string transformation function that can be used to transform input String before parsing. Useful when standards are not exactly respected and require a few tweaks
   */
  def sqlDateReads(pattern: String, corrector: String => String = identity): Reads[java.sql.Date] =
    dateReads(pattern, corrector).map(d => new java.sql.Date(d.getTime))

  /**
   * the default implicit SqlDate reads
   */
  implicit val DefaultSqlDateReads: Reads[java.sql.Date] = sqlDateReads("yyyy-MM-dd")

  /** Typeclass to implement way of parsing string as Java8 temporal types. */
  trait TemporalParser[T <: JTemporal] {
    def parse(input: String): Option[T]
  }

  /** Parsing companion */
  object TemporalParser {

    /** Instance of local date/time based on specified pattern. */
    implicit def LocalDateTimePatternParser(pattern: String): TemporalParser[LocalDateTime] =
      LocalDateTimeFormatterParser(DateTimeFormatter.ofPattern(pattern))

    /** Instance of local date/time based on formatter. */
    implicit def LocalDateTimeFormatterParser(formatter: DateTimeFormatter): TemporalParser[LocalDateTime] =
      new TemporalParser[LocalDateTime] {

        def parse(input: String): Option[LocalDateTime] =
          try {
            Some(LocalDateTime.parse(input, formatter))
          } catch {
            case _: DateTimeParseException           => None
            case _: UnsupportedTemporalTypeException => None
          }
      }

    /** Instance of offset date/time based on specified pattern. */
    implicit def OffsetDateTimePatternParser(pattern: String): TemporalParser[OffsetDateTime] =
      OffsetDateTimeFormatterParser(DateTimeFormatter.ofPattern(pattern))

    /** Instance of offset date/time based on formatter. */
    implicit def OffsetDateTimeFormatterParser(formatter: DateTimeFormatter): TemporalParser[OffsetDateTime] =
      new TemporalParser[OffsetDateTime] {

        def parse(input: String): Option[OffsetDateTime] =
          try {
            Some(OffsetDateTime.parse(input, formatter))
          } catch {
            case _: DateTimeParseException           => None
            case _: UnsupportedTemporalTypeException => None
          }
      }

    /** Instance of date based on specified pattern. */
    implicit def DatePatternParser(pattern: String): TemporalParser[LocalDate] =
      DateFormatterParser(DateTimeFormatter.ofPattern(pattern))

    /** Instance of date based on formatter. */
    implicit def DateFormatterParser(formatter: DateTimeFormatter): TemporalParser[LocalDate] =
      new TemporalParser[LocalDate] {

        def parse(input: String): Option[LocalDate] =
          try {
            Some(LocalDate.parse(input, formatter))
          } catch {
            case _: DateTimeParseException           => None
            case _: UnsupportedTemporalTypeException => None
          }
      }

    /** Instance of instant parser based on specified pattern. */
    implicit def InstantPatternParser(pattern: String): TemporalParser[Instant] =
      InstantFormatterParser(DateTimeFormatter.ofPattern(pattern))

    /** Instance of instant parser based on formatter. */
    implicit def InstantFormatterParser(formatter: DateTimeFormatter): TemporalParser[Instant] =
      new TemporalParser[Instant] {

        def parse(input: String): Option[Instant] =
          try {
            Some(Instant.from(formatter.parse(input)))
          } catch {
            case _: DateTimeParseException           => None
            case _: UnsupportedTemporalTypeException => None
          }
      }

    /** Instance of zoned date/time based on specified pattern. */
    implicit def ZonedDateTimePatternParser(pattern: String): TemporalParser[ZonedDateTime] =
      ZonedDateTimeFormatterParser(DateTimeFormatter.ofPattern(pattern))

    /** Instance of zoned date/time based on formatter. */
    implicit def ZonedDateTimeFormatterParser(formatter: DateTimeFormatter): TemporalParser[ZonedDateTime] =
      new TemporalParser[ZonedDateTime] {

        def parse(input: String): Option[ZonedDateTime] =
          try {
            Some(ZonedDateTime.parse(input, formatter))
          } catch {
            case _: DateTimeParseException           => None
            case _: UnsupportedTemporalTypeException => None
          }
      }

    /** Instance of LocalTime parser based on specified pattern. */
    implicit def LocalTimePatternParser(pattern: String): TemporalParser[LocalTime] =
      LocalTimeFormatterParser(DateTimeFormatter.ofPattern(pattern))

    /** Instance of LocalTime parser based on formatter. */
    implicit def LocalTimeFormatterParser(formatter: DateTimeFormatter): TemporalParser[LocalTime] =
      new TemporalParser[LocalTime] {

        def parse(input: String): Option[LocalTime] =
          try {
            Some(LocalTime.from(formatter.parse(input)))
          } catch {
            case _: DateTimeParseException           => None
            case _: UnsupportedTemporalTypeException => None
          }
      }
  }

  /**
   * @tparam A the parsing type
   * @tparam B the temporal type
   */
  private final class TemporalReads[A, B <: JTemporal](
      parsing: A,
      corrector: String => String,
      p: A => TemporalParser[B],
      epoch: Long => B
  ) extends Reads[B] {

    def reads(json: JsValue): JsResult[B] = json match {
      case n: JsNumber => n.validate[Long].map(epoch)
      case JsString(s) =>
        p(parsing).parse(corrector(s)) match {
          case Some(d) => JsSuccess(d)
          case None =>
            JsError(
              Seq(
                JsPath ->
                  Seq(JsonValidationError("error.expected.date.isoformat", parsing))
              )
            )
        }
      case _ =>
        JsError(
          Seq(
            JsPath ->
              Seq(JsonValidationError("error.expected.date"))
          )
        )
    }
  }

  /**
   * Reads for the `java.time.LocalDateTime` type.
   *
   * @tparam T Type of argument to instantiate date/time parser
   * @param parsing Argument to instantiate date/time parser. Actually either a pattern (string) or a formatter (`java.time.format.DateTimeFormatter`)
   * @param corrector a simple string transformation function that can be used to transform input String before parsing. Useful when standards are not exactly respected and require a few tweaks. Function `identity` can be passed if no correction is needed.
   * @param p Typeclass instance based on `parsing`
   * @see [[DefaultWrites.TemporalFormatter]]
   *
   * {{{
   * import java.time.format.DateTimeFormatter
   *
   * import play.api.libs.json.Reads.localDateTimeReads
   *
   * val customReads1 = localDateTimeReads("dd/MM/yyyy, HH:mm:ss")
   * val customReads2 = localDateTimeReads(DateTimeFormatter.ISO_DATE_TIME)
   * val customReads3 = localDateTimeReads(
   *   DateTimeFormatter.ISO_DATE_TIME, _.drop(1))
   * }}}
   */
  def localDateTimeReads[T](parsing: T, corrector: String => String = identity)(implicit
      p: T => TemporalParser[LocalDateTime]
  ): Reads[LocalDateTime] =
    new TemporalReads[T, LocalDateTime](
      parsing,
      corrector,
      p,
      { (millis: Long) => LocalDateTime.ofInstant(Instant.ofEpochMilli(millis), ZoneOffset.UTC) }
    )

  /**
   * The default typeclass to reads `java.time.LocalDateTime` from JSON.
   * Accepts date/time formats as '2011-12-03T10:15:30', '2011-12-03T10:15:30+01:00' or '2011-12-03T10:15:30+01:00[Europe/Paris]'.
   */
  implicit val DefaultLocalDateTimeReads: Reads[LocalDateTime] =
    localDateTimeReads(DateTimeFormatter.ISO_DATE_TIME)

  /**
   * Reads for the `java.time.OffsetDateTime` type.
   *
   * Note: it is intentionally not supported to read an OffsetDateTime
   * from a number.
   *
   * @tparam T the type of argument to instantiate date/time parser
   * @param parsing The argument to instantiate date/time parser. Actually either a pattern (string) or a formatter (`java.time.format.DateTimeFormatter`)
   * @param corrector a simple string transformation function that can be used to transform input String before parsing. Useful when standards are not exactly respected and require a few tweaks. Function `identity` can be passed if no correction is needed.
   * @param p the implicit conversion based on `parsing`
   * @see [[DefaultWrites.TemporalFormatter]]
   *
   * {{{
   * import java.time.format.DateTimeFormatter
   *
   * import play.api.libs.json.Reads.offsetDateTimeReads
   *
   * val customReads1 = offsetDateTimeReads("dd/MM/yyyy, HH:mm:ss (Z)")
   * val customReads2 = offsetDateTimeReads(DateTimeFormatter.ISO_OFFSET_DATE_TIME)
   * val customReads3 = offsetDateTimeReads(
   *   DateTimeFormatter.ISO_OFFSET_DATE_TIME, _.drop(1))
   * }}}
   */
  def offsetDateTimeReads[T](parsing: T, corrector: String => String = identity)(implicit
      p: T => TemporalParser[OffsetDateTime]
  ): Reads[OffsetDateTime] = new Reads[OffsetDateTime] {

    def reads(json: JsValue): JsResult[OffsetDateTime] = json match {
      case JsString(s) =>
        p(parsing).parse(corrector(s)) match {
          case Some(d) => JsSuccess(d)
          case None =>
            JsError(
              Seq(
                JsPath ->
                  Seq(JsonValidationError("error.expected.date.isoformat", parsing))
              )
            )
        }
      case _ =>
        JsError(
          Seq(
            JsPath ->
              Seq(JsonValidationError("error.expected.date"))
          )
        )
    }
  }

  /**
   * The default typeclass to reads `java.time.OffsetDateTime` from JSON.
   * Accepts date/time formats as '2011-12-03T10:15:30+01:00' or '2011-12-03T10:15:30+01:00[Europe/Paris]'.
   */
  implicit val DefaultOffsetDateTimeReads: Reads[OffsetDateTime] =
    offsetDateTimeReads(DateTimeFormatter.ISO_OFFSET_DATE_TIME)

  /**
   * Reads for the `java.time.ZonedDateTime` type.
   *
   * @tparam T Type of argument to instantiate date/time parser
   * @param parsing Argument to instantiate date/time parser. Actually either a pattern (string) or a formatter (`java.time.format.DateTimeFormatter`)
   * @param corrector a simple string transformation function that can be used to transform input String before parsing. Useful when standards are not exactly respected and require a few tweaks. Function `identity` can be passed if no correction is needed.
   * @param p Typeclass instance based on `parsing`
   * @see [[DefaultWrites.TemporalFormatter]]
   *
   * {{{
   * import java.time.format.DateTimeFormatter
   *
   * import play.api.libs.json.Reads.zonedDateTimeReads
   *
   * val customReads1 = zonedDateTimeReads("dd/MM/yyyy, HH:mm:ss")
   * val customReads2 = zonedDateTimeReads(DateTimeFormatter.ISO_DATE_TIME)
   * val customReads3 = zonedDateTimeReads(
   *   DateTimeFormatter.ISO_DATE_TIME, _.drop(1))
   * }}}
   */
  def zonedDateTimeReads[T](parsing: T, corrector: String => String = identity)(implicit
      p: T => TemporalParser[ZonedDateTime]
  ): Reads[ZonedDateTime] =
    new TemporalReads[T, ZonedDateTime](
      parsing,
      corrector,
      p,
      { (millis: Long) => ZonedDateTime.ofInstant(Instant.ofEpochMilli(millis), ZoneOffset.UTC) }
    )

  /**
   * The default typeclass to reads `java.time.ZonedDateTime` from JSON.
   * Accepts date/time formats as '2011-12-03T10:15:30', '2011-12-03T10:15:30+01:00' or '2011-12-03T10:15:30+01:00[Europe/Paris]'.
   */
  implicit val DefaultZonedDateTimeReads: Reads[ZonedDateTime] =
    zonedDateTimeReads(DateTimeFormatter.ISO_DATE_TIME)

  /**
   * Reads for the `java.time.LocalDate` type.
   *
   * @tparam T Type of argument to instantiate date parser
   * @param parsing Argument to instantiate date parser. Actually either a pattern (string) or a formatter (`java.time.format.DateTimeFormatter`)
   * @param corrector a simple string transformation function that can be used to transform input String before parsing. Useful when standards are not exactly respected and require a few tweaks. Function `identity` can be passed if no correction is needed.
   * @param p Typeclass instance based on `parsing`
   * @see [[DefaultWrites.TemporalFormatter]]
   *
   * {{{
   * import java.time.format.DateTimeFormatter
   *
   * import play.api.libs.json.Reads.localDateReads
   *
   * val customReads1 = localDateReads("dd/MM/yyyy, HH:mm:ss")
   * val customReads2 = localDateReads(DateTimeFormatter.ISO_DATE)
   * val customReads3 = localDateReads(DateTimeFormatter.ISO_DATE, _.drop(1))
   * }}}
   */
  def localDateReads[T](parsing: T, corrector: String => String = identity)(implicit
      p: T => TemporalParser[LocalDate]
  ): Reads[LocalDate] =
    new Reads[LocalDate] {

      def reads(json: JsValue): JsResult[LocalDate] = json match {
        case n: JsNumber => n.validate[Long].map(epoch)
        case JsString(s) =>
          p(parsing).parse(corrector(s)) match {
            case Some(d) => JsSuccess(d)
            case _ =>
              JsError(
                Seq(
                  JsPath ->
                    Seq(JsonValidationError("error.expected.date.isoformat", parsing))
                )
              )
          }
        case _ =>
          JsError(
            Seq(
              JsPath ->
                Seq(JsonValidationError("error.expected.date"))
            )
          )
      }

      @inline def epoch(millis: Long): LocalDate = LocalDate.now(
        Clock.fixed(Instant.ofEpochMilli(millis), ZoneOffset.UTC)
      )
    }

  /**
   * The default typeclass to reads `java.time.LocalDate` from JSON.
   * Accepts date formats as '2011-12-03'.
   */
  implicit val DefaultLocalDateReads: Reads[LocalDate] =
    localDateReads(DateTimeFormatter.ISO_DATE)

  /**
   * Reads for the `java.time.Instant` type.
   *
   * @tparam T Type of argument to instantiate date parser
   * @param parsing Argument to instantiate date parser. Actually either a pattern (string) or a formatter (`java.time.format.DateTimeFormatter`)
   * @param corrector a simple string transformation function that can be used to transform input String before parsing. Useful when standards are not exactly respected and require a few tweaks. Function `identity` can be passed if no correction is needed.
   * @param p Typeclass instance based on `parsing`
   * @see [[DefaultWrites.TemporalFormatter]]
   *
   * {{{
   * import java.time.format.DateTimeFormatter
   *
   * import play.api.libs.json.Reads.instantReads
   *
   * val customReads1 = instantReads("dd/MM/yyyy, HH:mm:ss")
   * val customReads2 = instantReads(DateTimeFormatter.ISO_INSTANT)
   * val customReads3 = instantReads(DateTimeFormatter.ISO_INSTANT, _.drop(1))
   * }}}
   */
  def instantReads[T](parsing: T, corrector: String => String = identity)(implicit
      p: T => TemporalParser[Instant]
  ): Reads[Instant] = new TemporalReads[T, Instant](parsing, corrector, p, Instant.ofEpochMilli(_))

  /**
   * The default typeclass to reads `java.time.Instant` from JSON.
   * Accepts instant formats as '2011-12-03T10:15:30Z', '2011-12-03T10:15:30+01:00' or '2011-12-03T10:15:30+01:00[Europe/Paris]'.
   */
  implicit val DefaultInstantReads: Reads[Instant] =
    instantReads(DateTimeFormatter.ISO_DATE_TIME)

  // ---

  /**
   * Reads for the `java.time.LocalTime` type.
   *
   * @tparam T Type of argument to instantiate time parser
   * @param parsing Argument to instantiate time parser. Actually either a pattern (string) or a formatter (`java.time.format.DateTimeFormatter`)
   * @param corrector a simple string transformation function that can be used to transform input String before parsing. Useful when standards are not exactly respected and require a few tweaks. Function `identity` can be passed if no correction is needed.
   * @param p Typeclass instance based on `parsing`
   * @see [[DefaultWrites.TemporalFormatter]]
   *
   * {{{
   * import java.time.format.DateTimeFormatter
   *
   * import play.api.libs.json.Reads.localTimeReads
   *
   * val customReads1 = localTimeReads("dd/MM/yyyy, HH:mm:ss")
   * val customReads2 = localTimeReads(DateTimeFormatter.ISO_TIME)
   * val customReads3 = localTimeReads(DateTimeFormatter.ISO_TIME, _.drop(1))
   * }}}
   */
  def localTimeReads[T](parsing: T, corrector: String => String = identity)(implicit
      p: T => TemporalParser[LocalTime]
  ): Reads[LocalTime] =
    new Reads[LocalTime] {

      def reads(json: JsValue): JsResult[LocalTime] = json match {
        case n: JsNumber => n.validate[Long].map(epoch)
        case JsString(s) =>
          p(parsing).parse(corrector(s)) match {
            case Some(d) => JsSuccess(d)
            case _ =>
              JsError(
                Seq(
                  JsPath ->
                    Seq(JsonValidationError("error.expected.date.isoformat", parsing))
                )
              )
          }
        case _ =>
          JsError(
            Seq(
              JsPath ->
                Seq(JsonValidationError("error.expected.date"))
            )
          )
      }

      @inline def epoch(nanos: Long): LocalTime = LocalTime.ofNanoOfDay(nanos)
    }

  /**
   * The default typeclass to reads `java.time.LocalTime` from JSON.
   * Accepts date formats as '10:15:30' (or '10:15').
   */
  implicit val DefaultLocalTimeReads: Reads[LocalTime] =
    localTimeReads(DateTimeFormatter.ISO_TIME)

  // ---

  /**
   * Reads for the `java.time.ZoneId` type.
   */
  implicit val ZoneIdReads: Reads[ZoneId] = Reads[ZoneId] {
    case JsString(s) =>
      try {
        JsSuccess(ZoneId.of(s))
      } catch {
        case _: DateTimeException => JsError(JsonValidationError("error.expected.timezone", s))
      }

    case _ => JsError(JsonValidationError("error.expected.jsstring"))
  }

  /** Deserializer for a `Locale` from a IETF BCP 47 string representation */
  implicit val localeReads: Reads[Locale] =
    Reads[Locale] {
      _.validate[String].flatMap(KeyReads.LanguageTagReads.readKey(_))
    }

  /** Deserializer for a `Locale` from an object representation */
  val localeObjectReads: Reads[Locale] = Reads[Locale] { json =>
    def base: JsResult[Locale] =
      (for {
        l <- (json \ "language").validate[String]
        c <- (json \ "country").validateOpt[String]
        v <- (json \ "variant").validateOpt[String]
      } yield (l, c, v)).flatMap {
        case (l, Some(country), Some(variant)) =>
          JsSuccess(new Locale(l, country, variant))

        case (l, Some(country), _) =>
          JsSuccess(new Locale(l, country))

        case (l, _, Some(_)) =>
          JsError("error.invalid.locale")

        case (l, _, _) => JsSuccess(new Locale(l))
      }

    base.flatMap { baseLocale =>
      for {
        ats <- (json \ "attributes").validateOpt[Set[String]]
        kws <- (json \ "keywords").validateOpt[Map[String, String]]
        spt <- (json \ "script").validateOpt[String]
        ext <- (json \ "extension").validateOpt(Reads.mapReads[Char, String] { s =>
          if (s.size == 1) JsSuccess(s.charAt(0))
          else JsError("error.invalid.character")
        })
      } yield {
        val builder = new Locale.Builder()

        builder.setLocale(baseLocale)

        ats.foreach(_.foreach { builder.addUnicodeLocaleAttribute(_) })

        kws.foreach(_.foreach {
          case (key, typ) =>
            builder.setUnicodeLocaleKeyword(key, typ)
        })

        ext.foreach(_.foreach {
          case (key, value) =>
            builder.setExtension(key, value)
        })

        spt.foreach { builder.setScript(_) }

        builder.build()
      }
    }
  }

  private def jdurationNumberReads(unit: TemporalUnit) =
    Reads[JDuration] {
      case n: JsNumber => n.validate[Long].map(l => JDuration.of(l, unit))
      case _           => JsError("error.expected.longDuration")
    }

  /**
   * Deserializer of Java Duration from an integer (long) number,
   * using the specified temporal unit.
   */
  def javaDurationNumberReads(unit: TemporalUnit): Reads[JDuration] =
    jdurationNumberReads(unit)

  /** Deserializer of Java Duration from a number of milliseconds. */
  val javaDurationMillisReads: Reads[JDuration] =
    javaDurationNumberReads(ChronoUnit.MILLIS)

  /**
   * Deserializer of Java Duration, from either a time-based amount of time
   * (string representation such as '34.5 seconds'),
   * or from a number of milliseconds (see [[javaDurationMillisReads]]).
   *
   * @see [[java.time.Duration]]
   * @see [[DefaultJavaPeriodReads]] if you want to use [[java.time.Period]] instead.
   */
  implicit val DefaultJavaDurationReads: Reads[JDuration] = Reads[JDuration] {
    case JsString(repr) =>
      try {
        JsSuccess(JDuration.parse(repr))
      } catch {
        case _: DateTimeParseException => JsError("error.invalid.duration")
      }

    case js => javaDurationMillisReads.reads(js)
  }

  /** Deserializer of Java Period from a number (integer) of days. */
  val javaPeriodDaysReads: Reads[Period] =
    Reads.IntReads.map(Period.ofDays)

  /** Deserializer of Java Period from a number (integer) of weeks. */
  val javaPeriodWeeksReads: Reads[Period] =
    Reads.IntReads.map(Period.ofWeeks)

  /** Deserializer of Java Period from a number (integer) of months. */
  val javaPeriodMonthsReads: Reads[Period] =
    Reads.IntReads.map(Period.ofMonths)

  /** Deserializer of Java Period from a number (integer) of years. */
  val javaPeriodYearsReads: Reads[Period] =
    Reads.IntReads.map(Period.ofYears)

  /**
   * Deserializer of Java Period, from either a time-based amount of time
   * in the ISO-8601 calendar system, such as '2 years, 3 months and 4 days'
   * or from a number of days (see [[javaPeriodDaysReads]]).
   *
   * @see [[java.time.Period]]
   * @see [[DefaultJavaDurationReads]] if you want to use [[java.time.Duration]] instead.
   */
  implicit val DefaultJavaPeriodReads: Reads[Period] = Reads[Period] {
    case JsString(repr) =>
      try {
        JsSuccess(Period.parse(repr))
      } catch {
        case _: DateTimeParseException => JsError("error.invalid.stringPeriod")
      }

    case js => javaPeriodDaysReads.reads(js)
  }

  protected def parseBigDecimal(input: String): JsResult[java.math.BigDecimal] =
    BigDecimalParser.parse(input, JsonParserSettings.settings)

  protected def parseBigInteger(input: String): JsResult[java.math.BigInteger] = {
    if (input.length > JsonParserSettings.settings.bigDecimalParseSettings.digitsLimit) {
      JsError("error.expected.numberdigitlimit")
    } else {
      try {
        JsSuccess(new java.math.BigInteger(input))
      } catch {
        case _: NumberFormatException =>
          JsError(JsonValidationError("error.expected.numberformatexception"))
      }
    }
  }
}

trait EnvKeyReads { self: KeyReads.type =>

  /**
   * Reads an object key as a locale, considering the key
   * to be a [[https://tools.ietf.org/html/rfc5646 language tag]].
   */
  implicit object LanguageTagReads extends KeyReads[Locale] {

    def readKey(key: String): JsResult[Locale] =
      try {
        JsSuccess(Locale.forLanguageTag(key))
      } catch {
        case NonFatal(cause) => JsError(JsonValidationError(Seq("error.expected.languageTag"), cause.getMessage))
      }
  }
}
