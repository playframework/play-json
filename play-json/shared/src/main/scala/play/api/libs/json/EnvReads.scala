package play.api.libs.json
import java.time.format.{ DateTimeFormatter, DateTimeParseException }
import java.time.temporal.{ ChronoUnit, TemporalUnit, UnsupportedTemporalTypeException, Temporal => JTemporal }
import java.time.{ Clock, DateTimeException, Instant, LocalDate, LocalDateTime, LocalTime, OffsetDateTime, Period, ZoneId, ZoneOffset, ZonedDateTime, Duration => JDuration }

trait EnvReads extends PlatformReads {
  import scala.language.implicitConversions

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
    implicit def LocalDateTimeFormatterParser(formatter: DateTimeFormatter): TemporalParser[LocalDateTime] = new TemporalParser[LocalDateTime] {
      def parse(input: String): Option[LocalDateTime] = try {
        Some(LocalDateTime.parse(input, formatter))
      } catch {
        case _: DateTimeParseException => None
        case _: UnsupportedTemporalTypeException => None
      }
    }

    /** Instance of offset date/time based on specified pattern. */
    implicit def OffsetDateTimePatternParser(pattern: String): TemporalParser[OffsetDateTime] =
      OffsetDateTimeFormatterParser(DateTimeFormatter.ofPattern(pattern))

    /** Instance of offset date/time based on formatter. */
    implicit def OffsetDateTimeFormatterParser(formatter: DateTimeFormatter): TemporalParser[OffsetDateTime] = new TemporalParser[OffsetDateTime] {
      def parse(input: String): Option[OffsetDateTime] = try {
        Some(OffsetDateTime.parse(input, formatter))
      } catch {
        case _: DateTimeParseException => None
        case _: UnsupportedTemporalTypeException => None
      }
    }

    /** Instance of date based on specified pattern. */
    implicit def DatePatternParser(pattern: String): TemporalParser[LocalDate] =
      DateFormatterParser(DateTimeFormatter.ofPattern(pattern))

    /** Instance of date based on formatter. */
    implicit def DateFormatterParser(formatter: DateTimeFormatter): TemporalParser[LocalDate] = new TemporalParser[LocalDate] {
      def parse(input: String): Option[LocalDate] = try {
        Some(LocalDate.parse(input, formatter))
      } catch {
        case _: DateTimeParseException => None
        case _: UnsupportedTemporalTypeException => None
      }
    }

    /** Instance of instant parser based on specified pattern. */
    implicit def InstantPatternParser(pattern: String): TemporalParser[Instant] =
      InstantFormatterParser(DateTimeFormatter.ofPattern(pattern))

    /** Instance of instant parser based on formatter. */
    implicit def InstantFormatterParser(formatter: DateTimeFormatter): TemporalParser[Instant] = new TemporalParser[Instant] {
      def parse(input: String): Option[Instant] = try {
        Some(Instant.from(formatter.parse(input)))
      } catch {
        case _: DateTimeException => None
        case _: DateTimeParseException => None
        case _: UnsupportedTemporalTypeException => None
      }
    }

    /** Instance of zoned date/time based on specified pattern. */
    implicit def ZonedDateTimePatternParser(pattern: String): TemporalParser[ZonedDateTime] = ZonedDateTimeFormatterParser(DateTimeFormatter.ofPattern(pattern))

    /** Instance of zoned date/time based on formatter. */
    implicit def ZonedDateTimeFormatterParser(formatter: DateTimeFormatter): TemporalParser[ZonedDateTime] = new TemporalParser[ZonedDateTime] {
      def parse(input: String): Option[ZonedDateTime] = try {
        Some(ZonedDateTime.parse(input, formatter))
      } catch {
        case _: DateTimeParseException => None
        case _: UnsupportedTemporalTypeException => None
      }
    }

    /** Instance of LocalTime parser based on specified pattern. */
    implicit def LocalTimePatternParser(pattern: String): TemporalParser[LocalTime] = LocalTimeFormatterParser(DateTimeFormatter.ofPattern(pattern))

    /** Instance of LocalTime parser based on formatter. */
    implicit def LocalTimeFormatterParser(formatter: DateTimeFormatter): TemporalParser[LocalTime] = new TemporalParser[LocalTime] {
      def parse(input: String): Option[LocalTime] = try {
        Some(LocalTime.from(formatter.parse(input)))
      } catch {
        case _: DateTimeParseException => None
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
      case JsNumber(d) => JsSuccess(epoch(d.toLong))
      case JsString(s) => p(parsing).parse(corrector(s)) match {
        case Some(d) => JsSuccess(d)
        case None => JsError(Seq(JsPath ->
          Seq(JsonValidationError("error.expected.date.isoformat", parsing))))
      }
      case _ => JsError(Seq(JsPath ->
        Seq(JsonValidationError("error.expected.date"))))
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
   * import play.api.libs.json.Reads.localDateTimeReads
   *
   * val customReads1 = localDateTimeReads("dd/MM/yyyy, HH:mm:ss")
   * val customReads2 = localDateTimeReads(DateTimeFormatter.ISO_DATE_TIME)
   * val customReads3 = localDateTimeReads(
   *   DateTimeFormatter.ISO_DATE_TIME, _.drop(1))
   * }}}
   */
  def localDateTimeReads[T](parsing: T, corrector: String => String = identity)(implicit p: T => TemporalParser[LocalDateTime]): Reads[LocalDateTime] = new TemporalReads[T, LocalDateTime](parsing, corrector, p, { millis: Long =>
    LocalDateTime.ofInstant(Instant.ofEpochMilli(millis), ZoneOffset.UTC)
  })

  /**
   * The default typeclass to reads `java.time.LocalDateTime` from JSON.
   * Accepts date/time formats as '2011-12-03T10:15:30', '2011-12-03T10:15:30+01:00' or '2011-12-03T10:15:30+01:00[Europe/Paris]'.
   */
  implicit val DefaultLocalDateTimeReads =
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
   * import play.api.libs.json.Reads.offsetDateTimeReads
   *
   * val customReads1 = offsetDateTimeReads("dd/MM/yyyy, HH:mm:ss (Z)")
   * val customReads2 = offsetDateTimeReads(DateTimeFormatter.ISO_OFFSET_DATE_TIME)
   * val customReads3 = offsetDateTimeReads(
   *   DateTimeFormatter.ISO_OFFSET_DATE_TIME, _.drop(1))
   * }}}
   */
  def offsetDateTimeReads[T](parsing: T, corrector: String => String = identity)(implicit p: T => TemporalParser[OffsetDateTime]): Reads[OffsetDateTime] = new Reads[OffsetDateTime] {
    def reads(json: JsValue): JsResult[OffsetDateTime] = json match {
      case JsString(s) => p(parsing).parse(corrector(s)) match {
        case Some(d) => JsSuccess(d)
        case None => JsError(Seq(JsPath ->
          Seq(JsonValidationError("error.expected.date.isoformat", parsing))))
      }
      case _ => JsError(Seq(JsPath ->
        Seq(JsonValidationError("error.expected.date"))))
    }
  }

  /**
   * The default typeclass to reads `java.time.OffsetDateTime` from JSON.
   * Accepts date/time formats as '2011-12-03T10:15:30+01:00' or '2011-12-03T10:15:30+01:00[Europe/Paris]'.
   */
  implicit val DefaultOffsetDateTimeReads =
    offsetDateTimeReads(DateTimeFormatter.ISO_OFFSET_DATE_TIME)

  /**
   * Reads for the `java.time.ZonedDateTime` type.
   *
   * @tparam T Type of argument to instantiate date/time parser
   * @param parsing Argument to instantiate date/time parser. Actually either a pattern (string) or a formatter (`java.time.format.DateTimeFormatter`)
   * @param corrector a simple string transformation function that can be used to transform input String before parsing. Useful when standards are not exactly respected and require a few tweaks. Function `identity` can be passed if no correction is needed.
   * @param p Typeclass instance based on `parsing`
   * @see [[DefaultWrites.TemporalFormatter]]
   * {{{
   * import play.api.libs.json.Reads.zonedDateTimeReads
   *
   * val customReads1 = zonedDateTimeReads("dd/MM/yyyy, HH:mm:ss")
   * val customReads2 = zonedDateTimeReads(DateTimeFormatter.ISO_DATE_TIME)
   * val customReads3 = zonedDateTimeReads(
   *   DateTimeFormatter.ISO_DATE_TIME, _.drop(1))
   * }}}
   */
  def zonedDateTimeReads[T](parsing: T, corrector: String => String = identity)(implicit p: T => TemporalParser[ZonedDateTime]): Reads[ZonedDateTime] = new TemporalReads[T, ZonedDateTime](parsing, corrector, p, { millis: Long =>
    ZonedDateTime.ofInstant(Instant.ofEpochMilli(millis), ZoneOffset.UTC)
  })

  /**
   * The default typeclass to reads `java.time.ZonedDateTime` from JSON.
   * Accepts date/time formats as '2011-12-03T10:15:30', '2011-12-03T10:15:30+01:00' or '2011-12-03T10:15:30+01:00[Europe/Paris]'.
   */
  implicit val DefaultZonedDateTimeReads =
    zonedDateTimeReads(DateTimeFormatter.ISO_DATE_TIME)

  /**
   * Reads for the `java.time.LocalDate` type.
   *
   * @tparam T Type of argument to instantiate date parser
   * @param parsing Argument to instantiate date parser. Actually either a pattern (string) or a formatter (`java.time.format.DateTimeFormatter`)
   * @param corrector a simple string transformation function that can be used to transform input String before parsing. Useful when standards are not exactly respected and require a few tweaks. Function `identity` can be passed if no correction is needed.
   * @param p Typeclass instance based on `parsing`
   * @see [[DefaultWrites.TemporalFormatter]]
   * {{{
   * import play.api.libs.json.Reads.localDateReads
   *
   * val customReads1 = localDateReads("dd/MM/yyyy, HH:mm:ss")
   * val customReads2 = localDateReads(DateTimeFormatter.ISO_DATE)
   * val customReads3 = localDateReads(DateTimeFormatter.ISO_DATE, _.drop(1))
   * }}}
   */
  def localDateReads[T](parsing: T, corrector: String => String = identity)(implicit p: T => TemporalParser[LocalDate]): Reads[LocalDate] =
    new Reads[LocalDate] {
      def reads(json: JsValue): JsResult[LocalDate] = json match {
        case JsNumber(d) => JsSuccess(epoch(d.toLong))
        case JsString(s) => p(parsing).parse(corrector(s)) match {
          case Some(d) => JsSuccess(d)
          case _ => JsError(Seq(JsPath ->
            Seq(JsonValidationError("error.expected.date.isoformat", parsing))))
        }
        case _ => JsError(Seq(JsPath ->
          Seq(JsonValidationError("error.expected.date"))))
      }

      @inline def epoch(millis: Long): LocalDate = LocalDate.now(
        Clock.fixed(Instant.ofEpochMilli(millis), ZoneOffset.UTC)
      )
    }

  /**
   * The default typeclass to reads `java.time.LocalDate` from JSON.
   * Accepts date formats as '2011-12-03'.
   */
  implicit val DefaultLocalDateReads =
    localDateReads(DateTimeFormatter.ISO_DATE)

  /**
   * Reads for the `java.time.Instant` type.
   *
   * @tparam T Type of argument to instantiate date parser
   * @param parsing Argument to instantiate date parser. Actually either a pattern (string) or a formatter (`java.time.format.DateTimeFormatter`)
   * @param corrector a simple string transformation function that can be used to transform input String before parsing. Useful when standards are not exactly respected and require a few tweaks. Function `identity` can be passed if no correction is needed.
   * @param p Typeclass instance based on `parsing`
   * @see [[DefaultWrites.TemporalFormatter]]
   * {{{
   * import play.api.libs.json.Reads.instantReads
   *
   * val customReads1 = instantReads("dd/MM/yyyy, HH:mm:ss")
   * val customReads2 = instantReads(DateTimeFormatter.ISO_INSTANT)
   * val customReads3 = instantReads(DateTimeFormatter.ISO_INSTANT, _.drop(1))
   * }}}
   */
  def instantReads[T](parsing: T, corrector: String => String = identity)(implicit p: T => TemporalParser[Instant]): Reads[Instant] = new TemporalReads[T, Instant](parsing, corrector, p, Instant.ofEpochMilli(_))

  /**
   * The default typeclass to reads `java.time.Instant` from JSON.
   * Accepts instant formats as '2011-12-03T10:15:30Z', '2011-12-03T10:15:30+01:00' or '2011-12-03T10:15:30+01:00[Europe/Paris]'.
   */
  implicit val DefaultInstantReads =
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
   * {{{
   * import play.api.libs.json.Reads.localTimeReads
   *
   * val customReads1 = localTimeReads("dd/MM/yyyy, HH:mm:ss")
   * val customReads2 = localTimeReads(DateTimeFormatter.ISO_TIME)
   * val customReads3 = localTimeReads(DateTimeFormatter.ISO_TIME, _.drop(1))
   * }}}
   */
  def localTimeReads[T](parsing: T, corrector: String => String = identity)(implicit p: T => TemporalParser[LocalTime]): Reads[LocalTime] =
    new Reads[LocalTime] {
      def reads(json: JsValue): JsResult[LocalTime] = json match {
        case JsNumber(d) => JsSuccess(epoch(d.toLong))
        case JsString(s) => p(parsing).parse(corrector(s)) match {
          case Some(d) => JsSuccess(d)
          case _ => JsError(Seq(JsPath ->
            Seq(JsonValidationError("error.expected.date.isoformat", parsing))))
        }
        case _ => JsError(Seq(JsPath ->
          Seq(JsonValidationError("error.expected.date"))))
      }

      @inline def epoch(nanos: Long): LocalTime = LocalTime.ofNanoOfDay(nanos)
    }

  /**
   * The default typeclass to reads `java.time.LocalTime` from JSON.
   * Accepts date formats as '10:15:30' (or '10:15').
   */
  implicit val DefaultLocalTimeReads =
    localTimeReads(DateTimeFormatter.ISO_TIME)

  // ---

  /**
   * Reads for the `java.time.ZoneId` type.
   */
  implicit val ZoneIdReads: Reads[ZoneId] = Reads[ZoneId] {
    case JsString(s) => try {
      JsSuccess(ZoneId.of(s))
    } catch {
      case _: DateTimeException => JsError(JsonValidationError("error.expected.timezone", s))
    }

    case _ => JsError(JsonValidationError("error.expected.jsstring"))
  }

  private def jdurationNumberReads(unit: TemporalUnit) =
    Reads[JDuration] {
      case JsNumber(n) if !n.ulp.isValidLong =>
        JsError("error.invalid.longDuration")

      case JsNumber(n) => JsSuccess(JDuration.of(n.toLong, unit))
      case _ => JsError("error.expected.lonDuration")
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
    case JsString(repr) => try {
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
    case JsString(repr) => try {
      JsSuccess(Period.parse(repr))
    } catch {
      case _: DateTimeParseException => JsError("error.invalid.stringPeriod")
    }

    case js => javaPeriodDaysReads.reads(js)
  }

}
