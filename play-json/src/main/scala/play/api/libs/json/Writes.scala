/*
 * Copyright (C) 2009-2016 Lightbend Inc. <https://www.lightbend.com>
 */
package play.api.libs.json

import java.time.format.DateTimeFormatter
import java.time.temporal.Temporal
import java.time.{ Instant, LocalDate, LocalDateTime, OffsetDateTime, ZoneOffset, ZonedDateTime, ZoneId }

import com.fasterxml.jackson.databind.JsonNode
import play.api.libs.functional.ContravariantFunctor
import play.api.libs.json.Json._
import play.api.libs.json.jackson.JacksonJson

import scala.annotation.implicitNotFound
import scala.collection._
import scala.reflect.ClassTag

/**
 * Json serializer: write an implicit to define a serializer for any type
 */
@implicitNotFound(
  "No Json serializer found for type ${A}. Try to implement an implicit Writes or Format for this type."
)
trait Writes[-A] {
  /**
   * Convert the object into a JsValue
   */
  def writes(o: A): JsValue

  /**
   * Transforms the resulting [[JsValue]] using transformer function.
   */
  def transform(transformer: JsValue => JsValue): Writes[A] = Writes[A] { a => transformer(this.writes(a)) }

  /**
   * Transforms the resulting [[JsValue]] using a `Writes[JsValue]`.
   */
  def transform(transformer: Writes[JsValue]): Writes[A] = Writes[A] { a => transformer.writes(this.writes(a)) }
}

@implicitNotFound(
  "No Json serializer as JsObject found for type ${A}. Try to implement an implicit OWrites or OFormat for this type."
)
trait OWrites[-A] extends Writes[A] {
  def writes(o: A): JsObject

  /**
   * Transforms the resulting [[JsObject]] using a transformer function.
   */
  def transform(transformer: JsObject => JsObject): OWrites[A] =
    OWrites[A] { a => transformer(this.writes(a)) }

  /**
   * Transforms the resulting [[JsValue]] using a `Writes[JsValue]`.
   */
  def transform(transformer: OWrites[JsObject]): OWrites[A] =
    OWrites[A] { a => transformer.writes(this.writes(a)) }

}

object OWrites extends PathWrites with ConstraintWrites {
  import play.api.libs.functional._

  implicit val functionalCanBuildOWrites: FunctionalCanBuild[OWrites] = new FunctionalCanBuild[OWrites] {

    def apply[A, B](wa: OWrites[A], wb: OWrites[B]): OWrites[A ~ B] = OWrites[A ~ B] { case a ~ b => wa.writes(a).deepMerge(wb.writes(b)) }

  }

  implicit val contravariantfunctorOWrites: ContravariantFunctor[OWrites] = new ContravariantFunctor[OWrites] {

    def contramap[A, B](wa: OWrites[A], f: B => A): OWrites[B] = OWrites[B](b => wa.writes(f(b)))

  }

  def apply[A](f: A => JsObject): OWrites[A] = new OWrites[A] {
    def writes(a: A): JsObject = f(a)
  }

  /**
   * Transforms the resulting [[JsObject]] using the given function,
   * which is also applied with the initial input.
   * def transform(transformer: (A, JsObject) => JsObject): OWrites[A] =
   * OWrites[A] { a => transformer(a, this.writes(a)) }
   *
   * @param w the initial writer
   * @param f the transformer function
   */
  def transform[A](w: OWrites[A])(f: (A, JsObject) => JsObject): OWrites[A] =
    OWrites[A] { a => f(a, w.writes(a)) }
}

/**
 * Default Serializers.
 */
object Writes extends PathWrites with ConstraintWrites with DefaultWrites {

  val constraints: ConstraintWrites = this
  val path: PathWrites = this

  implicit val contravariantfunctorWrites: ContravariantFunctor[Writes] =
    new ContravariantFunctor[Writes] {
      def contramap[A, B](wa: Writes[A], f: B => A): Writes[B] =
        Writes[B](b => wa.writes(f(b)))
    }

  def apply[A](f: A => JsValue): Writes[A] = new Writes[A] {
    def writes(a: A): JsValue = f(a)
  }

  /**
   * Transforms the resulting [[JsValue]] using the given function,
   * which is also applied with the initial input.
   * def transform(transformer: (A, JsValue) => JsValue): Writes[A] =
   * Writes[A] { a => transformer(a, this.writes(a)) }
   *
   * @param w the initial writer
   * @param f the transformer function
   */
  def transform[A](w: Writes[A])(f: (A, JsValue) => JsValue): Writes[A] =
    Writes[A] { a => f(a, w.writes(a)) }
}

/**
 * Default Serializers.
 */
trait DefaultWrites extends LowPriorityWrites {
  import scala.language.implicitConversions

  /**
   * Serializer for Int types.
   */
  implicit object IntWrites extends Writes[Int] {
    def writes(o: Int) = JsNumber(o)
  }

  /**
   * Serializer for Short types.
   */
  implicit object ShortWrites extends Writes[Short] {
    def writes(o: Short) = JsNumber(o)
  }

  /**
   * Serializer for Byte types.
   */
  implicit object ByteWrites extends Writes[Byte] {
    def writes(o: Byte) = JsNumber(o)
  }

  /**
   * Serializer for Long types.
   */
  implicit object LongWrites extends Writes[Long] {
    def writes(o: Long) = JsNumber(o)
  }

  /**
   * Serializer for Float types.
   */
  implicit object FloatWrites extends Writes[Float] {
    def writes(o: Float) = JsNumber(o)
  }

  /**
   * Serializer for Double types.
   */
  implicit object DoubleWrites extends Writes[Double] {
    def writes(o: Double) = JsNumber(o)
  }

  /**
   * Serializer for BigDecimal types.
   */
  implicit object BigDecimalWrites extends Writes[BigDecimal] {
    def writes(o: BigDecimal) = JsNumber(o)
  }

  /**
   * Serializer for Boolean types.
   */
  implicit object BooleanWrites extends Writes[Boolean] {
    def writes(o: Boolean) = JsBoolean(o)
  }

  /**
   * Serializer for String types.
   */
  implicit object StringWrites extends Writes[String] {
    def writes(o: String) = JsString(o)
  }

  /**
   * Serializer for Jackson JsonNode
   */
  implicit object JsonNodeWrites extends Writes[JsonNode] {
    def writes(o: JsonNode): JsValue = JacksonJson.jsonNodeToJsValue(o)
  }

  /**
   * Serializer for Array[T] types.
   */
  implicit def arrayWrites[T: ClassTag: Writes]: Writes[Array[T]] = Writes[Array[T]] { ts =>
    JsArray(ts.map(toJson(_)).toSeq)
  }

  /**
   * Serializer for Map[String,V] types.
   */
  implicit def mapWrites[V: Writes]: OWrites[Map[String, V]] = OWrites[Map[String, V]] { ts =>
    JsObject(ts.mapValues(toJson(_)).toSeq)
  }

  /**
   * Serializer for JsValues.
   */
  implicit object JsValueWrites extends Writes[JsValue] {
    def writes(o: JsValue) = o
  }

  /**
   * Serializer for Option.
   */
  implicit def OptionWrites[T](implicit fmt: Writes[T]): Writes[Option[T]] = new Writes[Option[T]] {
    def writes(o: Option[T]) = o match {
      case Some(value) => fmt.writes(value)
      case None => JsNull
    }
  }

  /**
   * Serializer for java.util.Date
   * @param pattern the pattern used by SimpleDateFormat
   */
  def dateWrites(pattern: String): Writes[java.util.Date] = new Writes[java.util.Date] {
    def writes(d: java.util.Date): JsValue = JsString(new java.text.SimpleDateFormat(pattern).format(d))
  }

  /**
   * Default Serializer java.util.Date -> JsNumber(d.getTime (nb of ms))
   */
  implicit object DefaultDateWrites extends Writes[java.util.Date] {
    def writes(d: java.util.Date): JsValue = JsNumber(d.getTime)
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
      DateTimeFormatter.ISO_LOCAL_DATE_TIME)

  /**
   * The default typeclass to write a `java.time.OffsetDateTime`,
   * using '2011-12-03T10:15:30+02:00' format.
   */
  implicit val DefaultOffsetDateTimeWrites =
    temporalWrites[OffsetDateTime, DateTimeFormatter](
      DateTimeFormatter.ISO_OFFSET_DATE_TIME)

  /**
   * The default typeclass to write a `java.time.ZonedDateTime`,
   * using '2011-12-03T10:15:30+01:00[Europe/Paris]' format.
   */
  implicit val DefaultZonedDateTimeWrites =
    temporalWrites[ZonedDateTime, DateTimeFormatter](
      DateTimeFormatter.ISO_ZONED_DATE_TIME)

  /**
   * The default typeclass to write a `java.time.LocalDate`,
   * using '2011-12-03' format.
   */
  implicit val DefaultLocalDateWrites =
    temporalWrites[LocalDate, DateTimeFormatter](
      DateTimeFormatter.ISO_LOCAL_DATE)

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
        t.toInstant(ZoneOffset.UTC).toEpochMilli))
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
      t.atStartOfDay.toInstant(ZoneOffset.UTC).toEpochMilli))
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

  /**
   * Serializer for java.sql.Date
   * @param pattern the pattern used by SimpleDateFormat
   */
  def sqlDateWrites(pattern: String): Writes[java.sql.Date] = new Writes[java.sql.Date] {
    def writes(d: java.sql.Date): JsValue = JsString(new java.text.SimpleDateFormat(pattern).format(d))
  }

  /**
   * Serializer for java.util.UUID
   */
  implicit object UuidWrites extends Writes[java.util.UUID] {
    def writes(u: java.util.UUID) = JsString(u.toString)
  }

  /**
   * Serializer for scala.Enumeration by name.
   */
  implicit def enumNameWrites[E <: Enumeration]: Writes[E#Value] = new Writes[E#Value] {
    def writes(value: E#Value): JsValue = JsString(value.toString)
  }

  /**
   * Serializer for Any, used for the args of errors.
   */
  private[json] object anyWrites extends Writes[Any] {
    def writes(a: Any): JsValue = a match {
      case s: String => JsString(s)
      case nb: Int => JsNumber(nb)
      case nb: Short => JsNumber(nb)
      case nb: Long => JsNumber(nb)
      case nb: Double => JsNumber(nb)
      case nb: Float => JsNumber(nb)
      case b: Boolean => JsBoolean(b)
      case js: JsValue => js
      case x => JsString(x.toString)
    }
  }
}

sealed trait LowPriorityWrites {
  /**
   * Serializer for Traversables types.
   */
  implicit def traversableWrites[A: Writes] = Writes[Traversable[A]] { as =>
    JsArray(as.map(toJson(_)).toSeq)
  } // Avoid resolution ambiguity with more specific Traversable Writes,
  // such as OWrites.map
}
