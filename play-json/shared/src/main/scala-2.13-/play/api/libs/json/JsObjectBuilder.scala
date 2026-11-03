/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import scala.collection.mutable.{ Builder => MBuilder }

private[json] final class JsObjectBuilder extends MBuilder[(String, Json.JsValueWrapper), JsObject] {

  // Keeps the fields in insertion order, as JsObject.apply does.
  private val fs = ImmutableLinkedHashMap.newBuilder[String, JsValue]

  def +=(elem: (String, Json.JsValueWrapper)): this.type = append(elem._1, elem._2)

  // Adds a field without the caller allocating a tuple.
  def append(key: String, value: Json.JsValueWrapper): this.type = {
    fs += (key -> Json.unwrap(value))

    this
  }

  override def ++=(xs: TraversableOnce[(String, Json.JsValueWrapper)]): this.type = {
    xs.foreach(`+=`)

    this
  }

  def knownSize: Int = fs.result().size

  def clear(): Unit = {
    fs.clear()
  }

  def result(): JsObject = JsObject(fs.result())
}
