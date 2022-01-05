/*
 * Copyright (C) 2009-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

object MapWrites {
  type Map[K, V] = scala.collection.Map[K, V]

  /**
   * Serializer for Map[String,V] types.
   */
  def mapWrites[V: Writes]: OWrites[Map[String, V]] = {
    val w = implicitly[Writes[V]]

    OWrites[Map[String, V]] { ts => JsObject(ts.mapValues(w.writes(_)).toSeq) }
  }
}
