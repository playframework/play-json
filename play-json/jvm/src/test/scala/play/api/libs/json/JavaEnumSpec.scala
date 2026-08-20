/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

final class JavaEnumSpec extends AnyWordSpec with Matchers with org.scalatestplus.scalacheck.ScalaCheckPropertyChecks {
  "Java Enum" should {
    "be supported with default string representation" when {
      def readsSpecs(r: Reads[JavaColorEnum]) = {
        r.reads(JsString("RED")).mustEqual(JsSuccess(JavaColorEnum.RED))
        r.reads(JsString("GREEN")).mustEqual(JsSuccess(JavaColorEnum.GREEN))
        r.reads(JsString("BLUE")).mustEqual(JsSuccess(JavaColorEnum.BLUE))

        r.reads(JsString("Red")).mustEqual(JsError("error.invalid.enum.JavaColorEnum"))
        r.reads(JsString("green")).mustEqual(JsError("error.invalid.enum.JavaColorEnum"))
        r.reads(JsString("BluE")).mustEqual(JsError("error.invalid.enum.JavaColorEnum"))
      }

      "read" in {
        readsSpecs(Json.javaEnumReads[JavaColorEnum])
      }

      def writesSpecs(w: Writes[JavaColorEnum]) = {
        w.writes(JavaColorEnum.RED).mustEqual(JsString("RED"))
        w.writes(JavaColorEnum.GREEN).mustEqual(JsString("GREEN"))
        w.writes(JavaColorEnum.BLUE).mustEqual(JsString("BLUE"))
      }

      "write" in {
        writesSpecs(implicitly[Writes[JavaColorEnum]])
      }

      "format" in {
        val f: Format[JavaColorEnum] = Json.javaEnumFormat

        readsSpecs(f)

        writesSpecs(f)
      }
    }
  }
}
