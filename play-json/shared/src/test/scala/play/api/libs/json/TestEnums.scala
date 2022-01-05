/*
 * Copyright (C) 2009-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

object TestEnums {

  object EnumWithCustomNames extends Enumeration {
    type EnumWithCustomNames = Value

    val (customEnum1, customEnum2) = (Value("ENUM1"), Value("ENUM2"))

    implicit val format: Format[EnumWithCustomNames] = Json.formatEnum(this)
  }

  object EnumWithDefaultNames extends Enumeration {
    type EnumWithDefaultNames = Value

    val defaultEnum1, defaultEnum2 = Value

    implicit val format: Format[EnumWithDefaultNames] = Json.formatEnum(this)
  }
}
