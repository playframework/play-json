/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import org.specs2.mutable._

import play.api.libs.json.Json._
import play.api.libs.functional.syntax._

class JsonValidSpec extends Specification {
  "JSON reads" should {
    "validate Dates" in {
      val d  = new java.util.Date()
      val df = new java.text.SimpleDateFormat("yyyy-MM-dd")
      val dd = df.parse(df.format(d))

      Json.toJson[java.util.Date](dd).validate[java.util.Date].must(beEqualTo(JsSuccess(dd)))
      JsNumber(dd.getTime).validate[java.util.Date].must(beEqualTo(JsSuccess(dd)))

      val ds = new java.sql.Date(dd.getTime())
      Json.toJson[java.sql.Date](ds).validate[java.sql.Date].must(beEqualTo(JsSuccess(dd)))
      JsNumber(dd.getTime).validate[java.sql.Date].must(beEqualTo(JsSuccess(dd)))

      // very poor test to do really crappy java date APIs
      // TODO ISO8601 test doesn't work on CI platform...
      val c = java.util.Calendar.getInstance()
      c.setTime(new java.util.Date(d.getTime - d.getTime % 1000))
      val js = {
        val fmt = new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssZ")
        JsString(fmt.format(c.getTime))
      }
      js.validate[java.util.Date](Reads.IsoDateReads).aka("formatted date").must(beEqualTo(JsSuccess(c.getTime)))
    }

    "verifyingIf reads" in {
      implicit val TupleReads: Reads[(String, JsObject)] = (
        (__ \ Symbol("type")).read[String] and
          (__ \ Symbol("data")).read(
            Reads.verifyingIf[JsObject] { case JsObject(fields) => !fields.isEmpty }(
              ((__ \ "title").read[String] and
                (__ \ "created").read[java.util.Date]).tupled
            )
          )
      ).tupled

      val d = (new java.util.Date()).getTime()
      Json
        .obj("type" -> "coucou", "data" -> Json.obj())
        .validate(TupleReads)
        .must(beEqualTo(JsSuccess("coucou" -> Json.obj())))
      Json
        .obj("type" -> "coucou", "data" -> Json.obj("title" -> "blabla", "created" -> d))
        .validate(TupleReads)
        .must(beEqualTo(JsSuccess("coucou" -> Json.obj("title" -> "blabla", "created" -> d))))
      Json
        .obj("type" -> "coucou", "data" -> Json.obj("title" -> "blabla"))
        .validate(TupleReads)
        .must(beEqualTo(JsError(__ \ "data" \ "created", "error.path.missing")))
    }

    "validate JsObject to Map without loosing precision" in {
      Json
        .obj("key1" -> 5.123, "key2" -> 3.543)
        .validate[Map[String, Float]]
        .mustEqual(JsSuccess(Map("key1" -> 5.123F, "key2" -> 3.543F)))
    }
  }
}
