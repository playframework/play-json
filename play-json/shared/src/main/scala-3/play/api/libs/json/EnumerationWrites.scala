/*
 * Copyright (C) 2009-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

private[json] trait EnumerationWrites {

  /** Serializer for scala.Enumeration by name. */
  given enumNameWrites[E <: Enumeration](using
      e: ValueOf[E]
  ): Writes[e.value.Value] =
    Writes[e.value.Value](value => JsString(value.toString))

}
