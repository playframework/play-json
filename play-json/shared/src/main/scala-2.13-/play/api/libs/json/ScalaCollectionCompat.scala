/*
 * Copyright (C) 2009-2019 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable

/** Copied from scala-collection-compat, because its binary API isn't stable. */
private[json] object ScalaCollectionCompat {

  /**
   * A factory that builds a collection of type `C` with elements of type `A`.
   *
   * Type param `A` is the type of elements (e.g. `Int`, `Boolean`, etc.).
   * Type param `C` is the type of collection (e.g. `List[Int]`, `TreeMap[Int, String]`, etc.).
   */
  type Factory[-A, +C] = CanBuildFrom[Nothing, A, C]

  final implicit class FactoryOps[-A, +C](private val factory: Factory[A, C]) {
    def newBuilder: mutable.Builder[A, C] = factory()
  }
}
