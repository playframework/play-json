/*
 * Copyright (C) 2009-2018 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json
import java.util.Locale

import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.node.{ ArrayNode, ObjectNode }
import play.api.libs.json.jackson.JacksonJson

trait PlatformReads {
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
      json.validate[JsObject] map (jo => JacksonJson.jsValueToJsonNode(jo).asInstanceOf[ObjectNode])
    }
  }

  /**
   * Deserializer for Jackson ArrayNode
   */
  implicit object ArrayNodeReads extends Reads[ArrayNode] {
    def reads(json: JsValue): JsResult[ArrayNode] = {
      json.validate[JsArray] map (ja => JacksonJson.jsValueToJsonNode(ja).asInstanceOf[ArrayNode])
    }
  }

  /**
   * Reads for the `java.util.Date` type.
   *
   * @param pattern a date pattern, as specified in `java.text.SimpleDateFormat`.
   * @param corrector a simple string transformation function that can be used to transform input String before parsing. Useful when standards are not exactly respected and require a few tweaks
   */
  def dateReads(pattern: String, corrector: String => String = identity): Reads[java.util.Date] = new Reads[java.util.Date] {

    def reads(json: JsValue): JsResult[java.util.Date] = json match {
      case JsNumber(d) => JsSuccess(new java.util.Date(d.toLong))
      case JsString(s) => parseJDate(pattern, corrector(s)) match {
        case Some(d) => JsSuccess(d)
        case None => JsError(Seq(JsPath ->
          Seq(JsonValidationError("error.expected.date.isoformat", pattern))))
      }
      case _ => JsError(Seq(JsPath ->
        Seq(JsonValidationError("error.expected.date"))))
    }
  }

  private def parseJDate(pattern: String, input: String): Option[java.util.Date] = {
    // REMEMBER THAT SIMPLEDATEFORMAT IS NOT THREADSAFE
    val df = new java.text.SimpleDateFormat(pattern)
    df.setLenient(false)
    try { Some(df.parse(input)) } catch {
      case x: java.text.ParseException =>
        None
    }
  }

  /**
   * the default implicit java.util.Date reads
   */
  implicit val DefaultDateReads = dateReads("yyyy-MM-dd")

  /**
   * ISO 8601 Reads
   */
  object IsoDateReads extends Reads[java.util.Date] {
    import java.util.Date

    val millisAndTz = "yyyy-MM-dd'T'HH:mm:ss.SSSX"
    val millis = "yyyy-MM-dd'T'HH:mm:ss.SSS"
    val tz = "yyyy-MM-dd'T'HH:mm:ssX"
    val mini = "yyyy-MM-dd'T'HH:mm:ss"

    val WithMillisAndTz = """^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}\.[0-9]{3}.+$""".r

    val WithMillis = """^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}\.[0-9]{3}$""".r

    val WithTz = """^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}[^.]+$""".r

    def reads(json: JsValue): JsResult[Date] = json match {
      case JsNumber(d) => JsSuccess(new Date(d.toLong))

      case JsString(s) => (s match {
        case WithMillisAndTz() => millisAndTz -> parseJDate(millisAndTz, s)
        case WithMillis() => millis -> parseJDate(millis, s)
        case WithTz() => tz -> parseJDate(tz, s)
        case _ => mini -> parseJDate(mini, s)
      }) match {
        case (_, Some(d)) => JsSuccess(d)
        case (p, None) => JsError(Seq(JsPath ->
          Seq(JsonValidationError("error.expected.date.isoformat", p))))
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
  implicit val DefaultSqlDateReads = sqlDateReads("yyyy-MM-dd")

  /** Deserializer for a `Locale` from a IETF BCP 47 string representation */
  implicit val localeReads: Reads[Locale] =
    Reads[Locale] { _.validate[String].map(Locale.forLanguageTag(_)) }

  /** Deserializer for a `Locale` from an object representation */
  val localeObjectReads: Reads[Locale] = Reads[Locale] { json =>
    def base: JsResult[Locale] = (for {
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
        ext <- (json \ "extension").validateOpt(
          Reads.mapReads[Char, String] { s =>
            if (s.size == 1) JsSuccess(s.charAt(0))
            else JsError("error.invalid.character")
          })
      } yield {
        val builder = new Locale.Builder()

        builder.setLocale(baseLocale)

        ats.foreach(_.foreach { builder.addUnicodeLocaleAttribute(_) })

        kws.foreach(_.foreach {
          case (key, typ) => builder.setUnicodeLocaleKeyword(key, typ)
        })

        ext.foreach(_.foreach {
          case (key, value) => builder.setExtension(key, value)
        })

        spt.foreach { builder.setScript(_) }

        builder.build()
      }
    }
  }

  // TODO: remove joda after 2.6.0
  import org.joda.time.format.{ DateTimeFormat, ISODateTimeFormat }
  import org.joda.time.{ DateTime, LocalDate, LocalTime }

  @deprecated("Include play-json-joda as a dependency and use JodaReads.jodaDateReads", "2.6.0")
  def jodaDateReads(pattern: String, corrector: String => String = identity): Reads[DateTime] = new Reads[DateTime] {
    val df = DateTimeFormat.forPattern(pattern)

    def reads(json: JsValue): JsResult[DateTime] = json match {
      case JsNumber(d) => JsSuccess(new DateTime(d.toLong))
      case JsString(s) => parseDate(corrector(s)) match {
        case Some(d) => JsSuccess(d)
        case _ => JsError(Seq(JsPath() -> Seq(JsonValidationError("error.expected.jodadate.format", pattern))))
      }
      case _ => JsError(Seq(JsPath() -> Seq(JsonValidationError("error.expected.date"))))
    }

    private def parseDate(input: String): Option[DateTime] =
      scala.util.control.Exception.allCatch[DateTime] opt (DateTime.parse(input, df))

  }

  @deprecated("Include play-json-joda as a dependency and use JodaReads.DefaultJodaDateTimeReads", "2.6.0")
  val DefaultJodaDateReads = jodaDateReads("yyyy-MM-dd")

  @deprecated("Include play-json-joda as a dependency and use JodaReads.jodaLocalDateReads", "2.6.0")
  def jodaLocalDateReads(pattern: String, corrector: String => String = identity): Reads[org.joda.time.LocalDate] = new Reads[org.joda.time.LocalDate] {

    val df = if (pattern == "") ISODateTimeFormat.localDateParser else DateTimeFormat.forPattern(pattern)

    def reads(json: JsValue): JsResult[LocalDate] = json match {
      case JsString(s) => parseDate(corrector(s)) match {
        case Some(d) => JsSuccess(d)
        case _ => JsError(Seq(JsPath() -> Seq(JsonValidationError("error.expected.jodadate.format", pattern))))
      }
      case _ => JsError(Seq(JsPath() -> Seq(JsonValidationError("error.expected.date"))))
    }

    private def parseDate(input: String): Option[LocalDate] =
      scala.util.control.Exception.allCatch[LocalDate] opt (LocalDate.parse(input, df))
  }

  @deprecated("Include play-json-joda as a dependency and use JodaReads.DefaultJodaLocalDateReads", "2.6.0")
  val DefaultJodaLocalDateReads = jodaLocalDateReads("")

  @deprecated("Include play-json-joda as a dependency and use JodaReads.jodaLocalTimeReads", "2.6.0")
  def jodaLocalTimeReads(pattern: String, corrector: String => String = identity): Reads[LocalTime] = new Reads[LocalTime] {

    val df = if (pattern == "") ISODateTimeFormat.localTimeParser else DateTimeFormat.forPattern(pattern)

    def reads(json: JsValue): JsResult[LocalTime] = json match {
      case JsNumber(n) => JsSuccess(new LocalTime(n.toLong))
      case JsString(s) => parseTime(corrector(s)) match {
        case Some(d) => JsSuccess(d)
        case None => JsError(Seq(JsPath() -> Seq(JsonValidationError("error.expected.jodatime.format", pattern))))
      }
      case _ => JsError(Seq(JsPath() -> Seq(JsonValidationError("error.expected.time"))))
    }

    private def parseTime(input: String): Option[LocalTime] =
      scala.util.control.Exception.allCatch[LocalTime] opt (LocalTime.parse(input, df))
  }

  @deprecated("Include play-json-joda as a dependency and use JodaReads.DefaultJodaLocalTimeReads", "2.6.0")
  val DefaultJodaLocalTimeReads = jodaLocalTimeReads("")
}
