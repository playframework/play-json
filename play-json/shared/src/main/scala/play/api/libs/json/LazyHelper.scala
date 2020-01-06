/*
 * Copyright (C) Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json.util

import scala.language.higherKinds

// Deprecated?
trait LazyHelper[M[_], T] {
  def lazyStuff: M[T]
}

object LazyHelper {
  def apply[M[_], T](stuff: M[T]) = new LazyHelper[M, T] {
    override lazy val lazyStuff = stuff
  }
}
