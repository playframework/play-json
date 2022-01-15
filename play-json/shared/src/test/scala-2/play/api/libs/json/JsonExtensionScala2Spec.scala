/*
 * Copyright (C) 2009-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import play.api.libs.json.JsonNaming.SnakeCase

import play.api.libs.json.Json._

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

case class WithDefault1(a: String = "a", b: Option[String] = Some("b"))
case class WithDefault2(a: String = "a", bar: Option[WithDefault1] = Some(WithDefault1()))

case class WithDefaultSnake(firstProp: String, defaultProp: String = "the default")

case class OptionalWithDefault(props: Option[String] = None)

class JsonExtensionScala2Spec extends AnyWordSpec with Matchers {
  "JsonExtensionScala2" should {
    "manage default values" should {
      import play.api.libs.json.Json
      import play.api.libs.functional.syntax._

      def functionalReads: Reads[WithDefault2] = {
        implicit val barReads: Reads[WithDefault1] = {
          (
            (__ \ "a").readWithDefault("a") and
              (__ \ "b").readNullableWithDefault(Some("b"))
          )(WithDefault1.apply _)
        }

        (
          (__ \ "a").readWithDefault("a") and
            (__ \ "bar").readNullableWithDefault(Some(WithDefault1()))
        )(WithDefault2.apply _)
      }

      def functionalFormat: Format[WithDefault2] = {
        implicit val barReads: Format[WithDefault1] = {
          (
            (__ \ "a").formatWithDefault("a") and
              (__ \ "b").formatNullableWithDefault(Some("b"))
          )(WithDefault1.apply, x => (x.a, x.b))
        }

        (
          (__ \ "a").formatWithDefault("a") and
            (__ \ "bar").formatNullableWithDefault(Some(WithDefault1()))
        )(WithDefault2.apply, x => (x.a, x.bar))
      }

      def macroReads: Reads[WithDefault2] = {
        implicit val br: Reads[WithDefault1] = Json.using[Json.WithDefaultValues].reads[WithDefault1]
        Json.using[MacroOptions with Json.DefaultValues].reads[WithDefault2]
      }

      def macroFormat: Format[WithDefault2] = {
        val jsWithDefaults                     = Json.using[Json.WithDefaultValues]
        implicit val bf: OFormat[WithDefault1] = jsWithDefaults.format[WithDefault1]

        jsWithDefaults.format[WithDefault2]
      }

      def validateReads(fooReads: Reads[WithDefault2]) = {
        fooReads.reads(Json.obj()).mustEqual(JsSuccess(WithDefault2()))
        fooReads.reads(Json.obj("a" -> JsNull)).mustEqual(JsSuccess(WithDefault2()))
        fooReads.reads(Json.obj("bar" -> JsNull)).mustEqual(JsSuccess(WithDefault2(bar = None)))
        fooReads.reads(Json.obj("a" -> "z")).mustEqual(JsSuccess(WithDefault2(a = "z")))
        fooReads
          .reads(Json.obj("a" -> "z", "bar" -> Json.obj("b" -> "z")))
          .mustEqual(JsSuccess(WithDefault2(a = "z", bar = Some(WithDefault1(b = Some("z"))))))
        fooReads
          .reads(Json.obj("a" -> 1))
          .mustEqual(JsError(List((JsPath \ "a") -> List(JsonValidationError("error.expected.jsstring")))))
      }

      "by functional reads" in validateReads(functionalReads)
      "by functional formats" in validateReads(functionalFormat)
      "by reads macro" in validateReads(macroReads)
      "by format macro" in validateReads(macroFormat)
    }

    "configuration methods" should {
      val json = Json.obj("first_prop" -> "the first")
      val data = WithDefaultSnake("the first")

      "allow supplying configuration via implicit config" in {
        implicit val config: JsonConfiguration.Aux[Json.WithDefaultValues] =
          JsonConfiguration[Json.WithDefaultValues](naming = SnakeCase)
        json.as(Json.reads[WithDefaultSnake]).mustEqual(data)
      }

      "allow supplying configuration via WithOptions" in {
        val config = JsonConfiguration[Json.WithDefaultValues](naming = SnakeCase)
        json.as(Json.configured(config).reads[WithDefaultSnake]).mustEqual(data)
      }
    }

    "create a Writes[OptionalWithDefault] with optionHandlers=WritesNull" in {
      implicit val jsonConfiguration: JsonConfiguration = JsonConfiguration(optionHandlers = OptionHandlers.WritesNull)
      val writer                                        = Json.writes[OptionalWithDefault]
      writer.writes(OptionalWithDefault()).mustEqual(Json.obj("props" -> JsNull))
    }

    "create a Format[OptionalWithDefault] with optionHandlers=WritesNull" in {
      implicit val jsonConfiguration: JsonConfiguration = JsonConfiguration(optionHandlers = OptionHandlers.WritesNull)
      val formatter                                     = Json.format[OptionalWithDefault]
      formatter.writes(OptionalWithDefault()).mustEqual(Json.obj("props" -> JsNull))
      formatter.writes(OptionalWithDefault(Some("foo"))).mustEqual(Json.obj("props" -> "foo"))

      formatter.reads(Json.obj()).mustEqual(JsSuccess(OptionalWithDefault()))
      formatter.reads(Json.obj("props" -> JsNull)).mustEqual(JsSuccess(OptionalWithDefault()))
      formatter.reads(Json.obj("props" -> Some("foo"))).mustEqual(JsSuccess(OptionalWithDefault(Some("foo"))))
    }

    "create a Writes[OptionalWithDefault] with DefaultValues and optionHandlers=WritesNull" in {
      implicit val jsonConfiguration: JsonConfiguration.Aux[Json.WithDefaultValues] =
        JsonConfiguration[Json.WithDefaultValues](optionHandlers = OptionHandlers.WritesNull)
      val writer = Json.writes[OptionalWithDefault]
      writer.writes(OptionalWithDefault()).mustEqual(Json.obj("props" -> JsNull))
    }

    "create a Format[OptionalWithDefault] with DefaultValues and optionHandlers=WritesNull" in {
      implicit val jsonConfiguration: JsonConfiguration.Aux[Json.WithDefaultValues] =
        JsonConfiguration[Json.WithDefaultValues](optionHandlers = OptionHandlers.WritesNull)
      val formatter = Json.format[OptionalWithDefault]
      formatter.writes(OptionalWithDefault()).mustEqual(Json.obj("props" -> JsNull))
      formatter.writes(OptionalWithDefault(Some("foo"))).mustEqual(Json.obj("props" -> "foo"))

      formatter.reads(Json.obj()).mustEqual(JsSuccess(OptionalWithDefault()))
      formatter.reads(Json.obj("props" -> JsNull)).mustEqual(JsSuccess(OptionalWithDefault(None)))
      formatter.reads(Json.obj("props" -> Some("foo"))).mustEqual(JsSuccess(OptionalWithDefault(Some("foo"))))
    }
  }
}
