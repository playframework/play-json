/*
 * Copyright (C) 2009-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package scalaguide.json

import play.api.libs.json.Json
import org.specs2.mutable.Specification

//#valueClass
final class IdText(val value: String) extends AnyVal
//#valueClass

class Scala2JsonAutomatedSpec extends Specification {
  "Scala 2 JSON automated" should {
    // lampepfl/dotty#7000 No Mirrors for value classes
    "for value class" >> {
      "produce a Reads" in {
        // #value-reads
        import play.api.libs.json._

        implicit val idTextReads: Reads[IdText] = Json.valueReads[IdText]
        // #value-reads

        JsString("foo").as[IdText].must_===(new IdText("foo"))
      }

      "produce a Writes" in {
        // #value-writes
        import play.api.libs.json._

        implicit val idTextWrites: Writes[IdText] = Json.valueWrites[IdText]
        // #value-writes

        Json.toJson(new IdText("bar")).must_===(JsString("bar"))
      }

      "produce a Format" in {
        // #value-format
        import play.api.libs.json._

        implicit val idTextFormat: Format[IdText] = Json.valueFormat[IdText]
        // #value-format

        val id = new IdText("lorem")

        JsString("lorem").as[IdText].must_===(id) and {
          Json.toJson(id).must_===(JsString("lorem"))
        }
      }
    }
  }
}
