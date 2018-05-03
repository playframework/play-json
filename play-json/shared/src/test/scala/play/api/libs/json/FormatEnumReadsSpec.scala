/*
 * Copyright (C) 2009-2018 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import org.scalatest._


class FormatEnumReadsSpec extends WordSpec with MustMatchers {

  import play.api.libs.json.TestEnums.EnumWithCustomNames._
  import play.api.libs.json.TestEnums.EnumWithDefaultNames._

  "EnumFormat" should {

    "deserialize correctly enum with custom names" in {

      JsString("ENUM1").validate[EnumWithCustomNames] mustEqual JsSuccess(customEnum1)
      JsString("ENUM2").validate[EnumWithCustomNames] mustEqual JsSuccess(customEnum2)

    }

    "deserialize correctly enum with default names" in {

      JsString("defaultEnum1").validate[EnumWithDefaultNames] mustEqual JsSuccess(defaultEnum1)
      JsString("defaultEnum2").validate[EnumWithDefaultNames] mustEqual JsSuccess(defaultEnum2)

    }

  }

}
