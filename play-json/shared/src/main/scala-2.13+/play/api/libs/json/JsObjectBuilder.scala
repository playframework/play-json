/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import scala.collection.mutable.{ Builder => MBuilder }

private[json] final class JsObjectBuilder extends MBuilder[(String, Json.JsValueWrapper), JsObject] {

  private val fs = Map.newBuilder[String, JsValue]

  def addOne(elem: (String, Json.JsValueWrapper)): this.type = {
    val (name, wrapped) = elem

    fs += (name -> Json.unwrap(wrapped))

    this
  }

  override def addAll(xs: IterableOnce[(String, Json.JsValueWrapper)]): this.type = {
    xs.iterator.foreach(addOne)

    this
  }

  override def knownSize: Int = fs.knownSize

  override def sizeHint(size: Int): Unit = {
    fs.sizeHint(size)
  }

  def clear(): Unit = {
    fs.clear()
  }

  def result(): JsObject = JsObject(fs.result())
}
