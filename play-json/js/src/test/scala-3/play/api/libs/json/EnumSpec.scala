/*
 * Copyright (C) 2009-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

final class EnumSpec extends AnyWordSpec with Matchers {
  "EnumFormat" should {
    import TestEnums.EnumWithDefaultNames._

    // https://gitter.im/lampepfl/dotty?at=5ee22d1e7b6da9126a8b4a51 ¯\_(ツ)_/¯
    "serialize correctly enum with default names" in {
      Json
        .toJson(defaultEnum1)
        .mustEqual(JsString("<Unknown name for enum field #0 of class class scala.Enumeration$Val>"))
      Json
        .toJson(defaultEnum2)
        .mustEqual(JsString("<Unknown name for enum field #1 of class class scala.Enumeration$Val>"))
    }

    "deserialize correctly enum with default names" in {
      val err1 = JsError(JsonValidationError("error.expected.validenumvalue"))
      val err2 = JsError(JsonValidationError("error.expected.validenumvalue"))
      JsString("defaultEnum1").validate[EnumWithDefaultNames].mustEqual(err1)
      JsString("defaultEnum2").validate[EnumWithDefaultNames].mustEqual(err2)
    }
  }
}
