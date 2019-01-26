/*
 * Copyright (C) 2009-2019 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import scala.collection.MapOps
import scala.collection.IterableOps

object MapWrites {

  type Map[K, V] = MapOps[K, V, ({ type l[X, +Y] = IterableOps[_, AnyConstr, _] })#l, _]

  //see scala.collection.AnyConstr
  private[json] type AnyConstr[X] = Any

  /**
   * Serializer for Map[String,V] types.
   */
  def mapWrites[V: Writes]: OWrites[Map[String, V]] = {
    val w = implicitly[Writes[V]]

    OWrites[Map[String, V]] { ts => JsObject(ts.mapValues(w.writes(_)).toSeq) }
  }
}
