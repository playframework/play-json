/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

private[json] trait JsValueCompat extends scala.deriving.Mirror.Sum {
  self: JsValue.type =>

  type MirroredType     = JsValue
  type MirroredMonoType = JsValue

  type MirroredElemTypes =
    JsNull.type *: JsBoolean *: JsNumber *: JsString *: JsArray *: JsObject *: EmptyTuple

  type MirroredElemLabels =
    "JsNull" *: "JsBoolean" *: "JsNumber" *: "JsString" *: "JsArray" *: "JsObject" *: EmptyTuple

  def ordinal(x: JsValue): Int =
    x match {
      case JsNull       => 0
      case _: JsBoolean => 1
      case _: JsNumber  => 2
      case _: JsString  => 3
      case _: JsArray   => 4
      case _: JsObject  => 5
    }
}

private[json] trait JsNumberCompat extends scala.deriving.Mirror.Product {
  self: JsNumber.type =>

  type MirroredType       = JsNumber
  type MirroredMonoType   = JsNumber
  type MirroredElemTypes  = BigDecimal *: EmptyTuple
  type MirroredElemLabels = "value" *: EmptyTuple

  def fromProduct(p: Product): JsNumber =
    new JsBigDecimal(p.productElement(0).asInstanceOf[BigDecimal])

}
