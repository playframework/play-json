/*
 * Copyright (C) 2009-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import java.util.{ LinkedHashMap => JLinkedHashMap }
import scala.collection.AbstractIterator
import scala.collection.MapFactory
import scala.collection.immutable.AbstractMap
import scala.collection.mutable

/**
 * Wraps a Java LinkedHashMap as a Scala immutable.Map.
 */
private[json] class ImmutableLinkedHashMap[A, +B](underlying: JLinkedHashMap[A, B]) extends AbstractMap[A, B] {

  override def get(key: A): Option[B] = Option(underlying.get(key))

  override def removed(key: A): Map[A, B] = {
    val c = shallowCopy()
    c.remove(key)
    new ImmutableLinkedHashMap(c)
  }

  override def updated[V1 >: B](key: A, value: V1): Map[A, V1] = {
    val c = shallowCopy[V1](size + 1)
    c.put(key, value)
    new ImmutableLinkedHashMap(c)
  }

  override def mapFactory: MapFactory[Map] = ImmutableLinkedHashMap

  override def iterator: Iterator[(A, B)] = new AbstractIterator[(A, B)] {
    private[this] val ui = underlying.entrySet().iterator()

    override def hasNext: Boolean = ui.hasNext

    override def knownSize: Int = if (underlying.isEmpty) 0 else super.knownSize

    override def next(): (A, B) = {
      val e = ui.next()
      (e.getKey, e.getValue)
    }
  }

  override def knownSize: Int = underlying.size()
  override def size: Int      = underlying.size()

  private def shallowCopy[V1 >: B](sizeHint: Int = size): JLinkedHashMap[A, V1] = {
    val c = new JLinkedHashMap[A, V1](sizeHint)
    for ((k, v) <- this) c.put(k, v)
    c
  }
}

private[json] object ImmutableLinkedHashMap extends MapFactory[Map] {
  private object EmptyMap extends ImmutableLinkedHashMap[Any, Nothing](new JLinkedHashMap(0))

  override def empty[K, V]: Map[K, V] = EmptyMap.asInstanceOf[Map[K, V]]

  override def from[K, V](it: IterableOnce[(K, V)]): Map[K, V] = (newBuilder ++= it).result()

  override def newBuilder[A, B]: mutable.Builder[(A, B), Map[A, B]] = new mutable.Builder[(A, B), Map[A, B]] {
    private[this] var lhm = new JLinkedHashMap[A, B](0)

    override def clear(): Unit = lhm.clear()

    override def sizeHint(size: Int): Unit = if (size > 0 && lhm.isEmpty) lhm = new JLinkedHashMap[A, B](size)

    override def result(): Map[A, B] = {
      if (lhm.isEmpty) empty
      else new ImmutableLinkedHashMap(lhm)
    }

    override def addOne(elem: (A, B)): this.type = {
      lhm.put(elem._1, elem._2)
      this
    }

    override def addAll(xs: IterableOnce[(A, B)]): this.type = {
      sizeHint(xs.knownSize)
      xs.iterator.foreach(addOne)
      this
    }
  }
}
