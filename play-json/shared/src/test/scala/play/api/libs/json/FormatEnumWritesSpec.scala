/*
 * Copyright (C) 2009-2018 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import org.scalatest._
import play.api.libs.json.Json._


class FormatEnumWritesSpec extends WordSpec with MustMatchers {

  import play.api.libs.json.TestEnums.EnumWithCustomNames._
  import play.api.libs.json.TestEnums.EnumWithDefaultNames._

  "EnumFormat" should {

    "serialize correctly enum with custom names" in {

      toJson(customEnum1) mustEqual JsString("ENUM1")
      toJson(customEnum2) mustEqual JsString("ENUM2")

    }

    "serialize correctly enum with default names" in {

      toJson(defaultEnum1) mustEqual JsString("defaultEnum1")
      toJson(defaultEnum2) mustEqual JsString("defaultEnum2")

    }

  }

}
