/*
 * Copyright (C) 2009-2018 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import java.util.Locale

import com.fasterxml.jackson.databind.JsonNode
import play.api.libs.json.jackson.JacksonJson

trait PlatformWrites {
  import scala.language.implicitConversions

  /**
   * Serializer for Jackson JsonNode
   */
  implicit object JsonNodeWrites extends Writes[JsonNode] {
    def writes(o: JsonNode): JsValue = JacksonJson.jsonNodeToJsValue(o)
  }

  /** Serializer for a `Locale` using the IETF BCP 47 string representation */
  implicit val localeWrites: Writes[Locale] =
    Writes[Locale] { l => JsString(l.toLanguageTag) }

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
        fields += "keywords" -> Json.toJson({
          val ks = Map.newBuilder[String, String]

          keywords.foreach { key =>
            Option(l.getUnicodeLocaleType(key)).foreach { typ =>
              ks += (key -> typ)
            }
          }

          ks.result()
        })
      }

      val extension = l.getExtensionKeys.asScala
      if (extension.nonEmpty) {
        fields += "extension" -> Json.toJson({
          val ext = Map.newBuilder[String, String]

          extension.foreach { key =>
            Option(l.getExtension(key)).foreach { v =>
              ext += (key.toString -> v)
            }
          }

          ext.result()
        })
      }

      JsObject(fields.result())
    }
  }

  // TODO: remove joda after 2.6.0
  import org.joda.time.{ DateTime, LocalDate, LocalTime }

  @deprecated("Include play-json-joda as a dependency and use JodaWrites.jodaDateWrites", "2.6.0")
  def jodaDateWrites(pattern: String): Writes[DateTime] = new Writes[DateTime] {
    val df = org.joda.time.format.DateTimeFormat.forPattern(pattern)
    def writes(d: DateTime): JsValue = JsString(d.toString(df))
  }

  @deprecated("Include play-json-joda as a dependency and use JodaWrites.JodaDateNumberWrites", "2.6.0")
  object DefaultJodaDateWrites extends Writes[DateTime] {
    def writes(d: DateTime): JsValue = JsNumber(d.getMillis)
  }

  @deprecated("Include play-json-joda as a dependency and use JodaWrites.jodaLocalDateWrites", "2.6.0")
  def jodaLocalDateWrites(pattern: String): Writes[LocalDate] = {
    val df = org.joda.time.format.DateTimeFormat.forPattern(pattern)
    Writes[LocalDate] { d => JsString(d.toString(df)) }
  }

  @deprecated("Include play-json-joda as a dependency and use JodaWrites.DefaultJodaLocalDateWrites", "2.6.0")
  object DefaultJodaLocalDateWrites extends Writes[LocalDate] {
    def writes(d: LocalDate): JsValue = JsString(d.toString)
  }

  @deprecated("Include play-json-joda as a dependency and use JodaWrites.jodaLocalTimeWrites", "2.6.0")
  def jodaLocalTimeWrites(pattern: String): Writes[LocalTime] =
    Writes[LocalTime] { d => JsString(d.toString(pattern)) }

  @deprecated("Include play-json-joda as a dependency and use JodaWrites.DefaultJodaLocalTimeWrites", "2.6.0")
  object DefaultJodaLocalTimeWrites extends Writes[LocalTime] {
    def writes(d: LocalTime): JsValue = JsString(d.toString)
  }
}
