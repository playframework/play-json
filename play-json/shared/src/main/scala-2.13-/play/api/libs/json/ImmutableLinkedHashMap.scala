/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import java.util.{ LinkedHashMap => JLinkedHashMap }
import scala.collection.generic.ImmutableMapFactory
import scala.collection.immutable.AbstractMap
import scala.collection.immutable.Map
import scala.collection.immutable.MapLike
import scala.collection.AbstractIterator
import scala.collection.GenTraversableOnce
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * Wraps a Java LinkedHashMap as a Scala immutable.Map.
 */
@SerialVersionUID(-2338626292552177485L)
private[json] class ImmutableLinkedHashMap[A, +B](underlying: JLinkedHashMap[A, B])
    extends AbstractMap[A, B]
    with Map[A, B]
    with MapLike[A, B, ImmutableLinkedHashMap[A, B]]
    with Serializable {

  override def get(key: A): Option[B] = Option(underlying.get(key))

  override def +[V1 >: B](kv: (A, V1)): Map[A, V1] = {
    val c = shallowCopy[V1](if (underlying.containsKey(kv._1)) size else size + 1)
    c.put(kv._1, kv._2)
    new ImmutableLinkedHashMap(c)
  }

  override def ++[V1 >: B](xs: GenTraversableOnce[(A, V1)]): Map[A, V1] = {
    val c = shallowCopy[V1]()
    xs.foreach { case (k, v) => c.put(k, v) }
    new ImmutableLinkedHashMap(c)
  }

  override def -(key: A) = {
    val c = shallowCopy[B]()
    c.remove(key)
    new ImmutableLinkedHashMap[A, B](c)
  }

  override def iterator: Iterator[(A, B)] = new AbstractIterator[(A, B)] {
    private[this] val ui = underlying.entrySet().iterator()

    override def hasNext: Boolean = ui.hasNext

    override def next(): (A, B) = {
      val e = ui.next()
      (e.getKey, e.getValue)
    }
  }

  override def size: Int = underlying.size()

  override def empty = ImmutableLinkedHashMap.empty

  // Sized for sizeHint entries, so the copy never rehashes, and copied from the entries
  // directly rather than through the tuple iterator.
  private def shallowCopy[V1 >: B](sizeHint: Int = size): JLinkedHashMap[A, V1] = {
    val c  = new JLinkedHashMap[A, V1](ImmutableLinkedHashMap.capacityFor(sizeHint))
    val it = underlying.entrySet().iterator()

    while (it.hasNext) {
      val e = it.next()
      c.put(e.getKey, e.getValue)
    }

    c
  }
}

private[json] object ImmutableLinkedHashMap extends ImmutableMapFactory[ImmutableLinkedHashMap] {
  private object EmptyMap extends ImmutableLinkedHashMap[Any, Nothing](new JLinkedHashMap(0))

  // Smallest initial capacity that holds `size` entries at the default 0.75 load factor
  // (what LinkedHashMap.newLinkedHashMap computes on JDK 19+, not available at --release 17).
  private[json] def capacityFor(size: Int): Int = math.ceil(size / 0.75).toInt

  override def empty[A, B]: ImmutableLinkedHashMap[A, B] = EmptyMap.asInstanceOf[ImmutableLinkedHashMap[A, B]]

  override def newBuilder[A, B]: mutable.Builder[(A, B), ImmutableLinkedHashMap[A, B]] = {
    ArrayBuffer
      .newBuilder[(A, B)]
      .mapResult { buf =>
        // buffering makes the size knowable resulting in a compact final hashmap
        // in practice, most objects are much smaller than LHM.DEFAULT_INITIAL_CAPACITY=16.
        if (buf.isEmpty) ImmutableLinkedHashMap.empty
        else {
          val lhm = new JLinkedHashMap[A, B](buf.size)
          buf.foreach(t => lhm.put(t._1, t._2))
          new ImmutableLinkedHashMap(lhm)
        }
      }
  }
}
