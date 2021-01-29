/*
 * Copyright (C) 2009-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

private[json] trait EnumerationWrites {

  /** Serializer for scala.Enumeration by name. */
  implicit def enumNameWrites[E <: Enumeration]: Writes[E#Value] =
    Writes[E#Value](value => JsString(value.toString))

}
