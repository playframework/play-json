/*
 * Copyright (C) 2009-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import org.scalacheck.Gen

class MacroScala2Spec extends AnyWordSpec with Matchers with org.scalatestplus.scalacheck.ScalaCheckPropertyChecks {
  import MacroScala2Spec._

  "Macro" should {
    // lampepfl/dotty-feature-requests#163 No Mirrors in Scala 3 for case classes with implicits
    "handle case class with implicits" when {
      val json1    = Json.obj("pos" -> 2, "text" -> "str")
      val json2    = Json.obj("ident" -> "id", "value" -> 23.456D)
      val fixture1 = WithImplicit1(2, "str")
      val fixture2 = WithImplicit2("id", 23.456D)

      def readSpec1(r: Reads[WithImplicit1]) =
        r.reads(json1).mustEqual(JsSuccess(fixture1))

      def writeSpec2(w: OWrites[WithImplicit2[Double]]) =
        w.writes(fixture2).mustEqual(json2)

      "to generate Reads" in readSpec1(Json.reads[WithImplicit1])

      "to generate Writes with type parameters" in writeSpec2(
        Json.writes[WithImplicit2[Double]]
      )

      "to generate Format" in {
        val f1 = Json.format[WithImplicit1]
        val f2 = Json.format[WithImplicit2[Double]]

        readSpec1(f1)
        f1.writes(fixture1).mustEqual(json1)
        writeSpec2(f2)
        f2.reads(json2).mustEqual(JsSuccess(fixture2))
      }
    }

    // lampepfl/dotty#7000 No Mirrors for value classes
    "handle ValueClass" in {
      val id                           = new TextId("foo")
      val js                           = JsString("foo")
      implicit val fmt: Format[TextId] = Json.valueFormat[TextId]

      js.validate[TextId].mustEqual(JsSuccess(id))
      fmt.writes(id).mustEqual(js)
    }
  }
}

object MacroScala2Spec {

  case class WithImplicit1(pos: Int, text: String)(implicit
      x: Numeric[Int]
  ) { def x1 = x.one }
  case class WithImplicit2[N: Numeric](ident: String, value: N)
}
