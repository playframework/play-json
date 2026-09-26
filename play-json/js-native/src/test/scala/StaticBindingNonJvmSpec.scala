/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

final class StaticBindingNonJvmSpec extends AnyWordSpec with Matchers {
  "JSON value" should {
    "be converted" when {
      import StaticBindingNonJvm.fromJs

      "it's a number" in {
        fromJs(JsNumber(123.45D), true, 0, _ => "") mustEqual "123.45"
      }
    }
  }
}
