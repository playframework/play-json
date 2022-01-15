/*
 * Copyright (C) 2009-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import org.joda.time._
import org.joda.time.format._

object JodaReads extends JodaReads

trait JodaReads {

  /**
   * Reads for the `org.joda.time.DateTime` type.
   *
   * @param pattern a date pattern, as specified in `org.joda.time.format.DateTimeFormat`, or "" to use ISO format.
   * @param corrector a simple string transformation function that can be used to transform input String before parsing.
   *                  Useful when standards are not respected and require a few tweaks. Defaults to identity function.
   */
  def jodaDateReads(pattern: String, corrector: String => String = identity): Reads[DateTime] = new Reads[DateTime] {
    val df = if (pattern == "") ISODateTimeFormat.dateOptionalTimeParser else DateTimeFormat.forPattern(pattern)

    def reads(json: JsValue): JsResult[DateTime] = json match {
      case JsNumber(d) => JsSuccess(new DateTime(d.toLong))
      case JsString(s) =>
        parseDate(corrector(s)) match {
          case Some(d) => JsSuccess(d)
          case _       => JsError(Seq(JsPath() -> Seq(JsonValidationError("error.expected.jodadate.format", pattern))))
        }
      case _ => JsError(Seq(JsPath() -> Seq(JsonValidationError("error.expected.date"))))
    }

    private def parseDate(input: String): Option[DateTime] =
      scala.util.control.Exception.nonFatalCatch[DateTime].opt(DateTime.parse(input, df))
  }

  /**
   * The default implicit JodaDate reads, using yyyy-MM-dd format
   */
  val JodaDateReads = jodaDateReads("yyyy-MM-dd")

  /**
   * The default implicit JodaDate reads, using ISO-8601 format
   */
  implicit val DefaultJodaDateTimeReads: Reads[DateTime] = jodaDateReads("")

  /**
   * Reads for the `org.joda.time.LocalDate` type.
   *
   * @param pattern a date pattern, as specified in `org.joda.time.format.DateTimeFormat`, or "" to use ISO format.
   * @param corrector string transformation function (See jodaDateReads). Defaults to identity function.
   */
  def jodaLocalDateReads(pattern: String, corrector: String => String = identity): Reads[org.joda.time.LocalDate] =
    new Reads[org.joda.time.LocalDate] {
      val df = if (pattern == "") ISODateTimeFormat.localDateParser else DateTimeFormat.forPattern(pattern)

      def reads(json: JsValue): JsResult[LocalDate] = json match {
        case JsString(s) =>
          parseDate(corrector(s)) match {
            case Some(d) => JsSuccess(d)
            case _ => JsError(Seq(JsPath() -> Seq(JsonValidationError("error.expected.jodadate.format", pattern))))
          }
        case _ => JsError(Seq(JsPath() -> Seq(JsonValidationError("error.expected.date"))))
      }

      private def parseDate(input: String): Option[LocalDate] =
        scala.util.control.Exception.nonFatalCatch[LocalDate].opt(LocalDate.parse(input, df))
    }

  /**
   * The default implicit joda.time.LocalDate reads, using ISO-8601 format.
   */
  implicit val DefaultJodaLocalDateReads: Reads[LocalDate] = jodaLocalDateReads("")

  /**
   * Reads for the `org.joda.time.LocalTime` type.
   *
   * @param pattern a date pattern, as specified in `org.joda.time.format.DateTimeFormat`, or "" to use ISO format.
   * @param corrector string transformation function (See jodaTimeReads). Defaults to identity function.
   */
  def jodaLocalTimeReads(pattern: String, corrector: String => String = identity): Reads[LocalTime] =
    new Reads[LocalTime] {
      val df = if (pattern == "") ISODateTimeFormat.localTimeParser else DateTimeFormat.forPattern(pattern)

      def reads(json: JsValue): JsResult[LocalTime] = json match {
        case JsNumber(n) => JsSuccess(new LocalTime(n.toLong))
        case JsString(s) =>
          parseTime(corrector(s)) match {
            case Some(d) => JsSuccess(d)
            case None => JsError(Seq(JsPath() -> Seq(JsonValidationError("error.expected.jodatime.format", pattern))))
          }
        case _ => JsError(Seq(JsPath() -> Seq(JsonValidationError("error.expected.time"))))
      }

      private def parseTime(input: String): Option[LocalTime] =
        scala.util.control.Exception.nonFatalCatch[LocalTime].opt(LocalTime.parse(input, df))
    }

  /**
   * the default implicit joda.time.LocalTime reads
   */
  implicit val DefaultJodaLocalTimeReads: Reads[LocalTime] = jodaLocalTimeReads("")
}
