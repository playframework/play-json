/*
 * Copyright (C) 2009-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

final class MacroScala3Spec
    extends AnyWordSpec
    with Matchers
    with org.scalatestplus.scalacheck.ScalaCheckPropertyChecks {
  "Case class" should {
    "not be handled" when {
      "no Product Conversion" in {
        import MacroSpec.UsingAlias

        "Macros.writer[UsingAlias]".mustNot(typeCheck)
      }

      "no custom ProductOf" in {
        "Macros.writer[CustomNoProductOf]".mustNot(typeCheck)
      }
    }
  }
}

final class CustomNoProductOf(val name: String, val age: Int)

object CustomNoProductOf {

  given Conversion[CustomNoProductOf, Tuple2[String, Int]] =
    (v: CustomNoProductOf) => v.name -> v.age
}
