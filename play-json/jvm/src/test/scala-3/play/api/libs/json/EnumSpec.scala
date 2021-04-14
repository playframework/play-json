/*
 * Copyright (C) 2009-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import ScalaTestPosition._

final class EnumSpec extends AnyWordSpec with Matchers {
  "EnumFormat" should {
    import TestEnums.EnumWithDefaultNames._

    "serialize correctly enum with default names" in {
      Json.toJson(defaultEnum1).mustEqual(JsString("defaultEnum1"))
      Json.toJson(defaultEnum2).mustEqual(JsString("defaultEnum2"))
    }

    "deserialize correctly enum with default names" in {
      JsString("defaultEnum1").validate[EnumWithDefaultNames].mustEqual(JsSuccess(defaultEnum1))
      JsString("defaultEnum2").validate[EnumWithDefaultNames].mustEqual(JsSuccess(defaultEnum2))
    }
  }
}
