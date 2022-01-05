/*
 * Copyright (C) 2009-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import org.joda.time._

object JodaWrites extends JodaWrites

trait JodaWrites {

  /**
   * Serializer for DateTime
   * @param pattern the pattern used by org.joda.time.format.DateTimeFormat
   */
  def jodaDateWrites(pattern: String): Writes[DateTime] = new Writes[DateTime] {
    val df                           = org.joda.time.format.DateTimeFormat.forPattern(pattern)
    def writes(d: DateTime): JsValue = JsString(d.toString(df))
  }

  /**
   * Serializer DateTime -> JsNumber(d.getMillis (number of milliseconds since the Epoch))
   */
  object JodaDateTimeNumberWrites extends Writes[DateTime] {
    def writes(d: DateTime): JsValue = JsNumber(d.getMillis)
  }

  /**
   * Default Serializer LocalDate -> JsString(ISO8601 format (yyyy-MM-dd))
   */
  implicit object JodaDateTimeWrites extends Writes[DateTime] {
    def writes(d: DateTime): JsValue = JsString(d.toString)
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
}
