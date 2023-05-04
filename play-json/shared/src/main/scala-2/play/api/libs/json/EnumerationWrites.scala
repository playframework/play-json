/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

private[json] trait EnumerationWrites {

  /** Serializer for scala.Enumeration by name. */
  implicit def enumNameWrites[E <: Enumeration]: Writes[E#Value] =
    Writes[E#Value](value => JsString(value.toString))

}
