/*
 * Copyright (C) 2009-2020 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

/** Copied from scala-collection-compat, because its binary API isn't stable. */
private[json] object ScalaCollectionCompat {
  type Factory[-A, +C] = scala.collection.Factory[A, C]
}
