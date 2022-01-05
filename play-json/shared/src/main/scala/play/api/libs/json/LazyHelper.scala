/*
 * Copyright (C) 2009-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json.util

// Deprecated?
trait LazyHelper[M[_], T] {
  def lazyStuff: M[T]
}

object LazyHelper {

  def apply[M[_], T](stuff: M[T]) = new LazyHelper[M, T] {
    override lazy val lazyStuff = stuff
  }
}
