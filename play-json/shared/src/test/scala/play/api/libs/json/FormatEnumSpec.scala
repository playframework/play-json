/*
 * Copyright (C) 2009-2018 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import org.scalatest._
import play.api.libs.json.Json._

object EnumWithCustomNames extends Enumeration {
  type EnumWithCustomNames = Value

  val (customEnum1, customEnum2) = (Value("ENUM1"), Value("ENUM2"))

  implicit val format: Format[EnumWithCustomNames] = Json.formatEnum(this)
}

object EnumWithDefaultNames extends Enumeration {
  type EnumWithDefaultNames = Value

  val (defaultEnum1, defaultEnum2) = (Value, Value)

  implicit val format: Format[EnumWithDefaultNames] = Json.formatEnum(this)
}

class FormatEnumSpec extends WordSpec with MustMatchers {

  import EnumWithCustomNames._
  import EnumWithDefaultNames._

  "EnumFormat" should {

    "serialize correctly enum with custom names" in {

      toJson(customEnum1).toString mustEqual """"ENUM1""""
      toJson(customEnum2).toString mustEqual """"ENUM2""""

    }

    "serialize correctly enum with default names" in {

      toJson(defaultEnum1).toString mustEqual """"defaultEnum1""""
      toJson(defaultEnum2).toString mustEqual """"defaultEnum2""""

    }

    "deserialize correctly enum with custom names" in {

      parse(""""ENUM1"""").validate[EnumWithCustomNames].get mustEqual customEnum1
      parse(""""ENUM2"""").validate[EnumWithCustomNames].get mustEqual customEnum2

    }

    "deserialize correctly enum with default names" in {

      parse(""""defaultEnum1"""").validate[EnumWithDefaultNames].get mustEqual defaultEnum1
      parse(""""defaultEnum2"""").validate[EnumWithDefaultNames].get mustEqual defaultEnum2

    }

  }

}
