/*
 * Copyright (C) 2009-2018 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import play.api.libs.json._
import play.api.libs.json.Json._

import scala.util.control.Exception._
import java.text.ParseException

import org.scalatest._

class JsonRichSpec extends WordSpec with MustMatchers {
  "JSON" should {
    "create json with rich syntax" in {
      def js = Json.obj(
        "key1" -> Json.obj("key11" -> "value11", "key12" -> 123L, "key13" -> JsNull),
        "key2" -> 123,
        "key3" -> true,
        "key4" -> Json.arr("value41", 345.6, JsString("test"), JsObject(Seq("key411" -> obj("key4111" -> 987.654)))),
        "url" -> Option(new java.net.URL("http://a.b")).map(_.toString).getOrElse("#")
      )

      js mustEqual (
        JsObject(Seq(
          "key1" -> JsObject(Seq(
            "key11" -> JsString("value11"),
            "key12" -> JsNumber(123L),
            "key13" -> JsNull
          )),
          "key2" -> JsNumber(123),
          "key3" -> JsTrue,
          "key4" -> JsArray(Array(
            JsString("value41"), JsNumber(345.6),
            JsString("test"), JsObject(Seq("key411" -> JsObject(Seq("key4111" -> JsNumber(987.654)))))
          ))
        ))
      )
    }
  }
}
