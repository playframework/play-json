/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import play.api.libs.json.Json._

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class JsLookupSpec extends AnyWordSpec with Matchers {
  "JsLookupResult" should {
    val obj = Json.obj(
      "field" -> 123
    )
    val result = obj \ "missingField"

    "return JsUndefined when a key is missing" in {
      result mustBe a[JsUndefined]
    }

    "return None when calling asOpt on JsUndefined" in {
      result.asOpt[Int].mustEqual(None)
    }
  }
}
