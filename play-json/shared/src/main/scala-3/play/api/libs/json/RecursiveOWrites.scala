/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import scala.annotation.tailrec

trait RecursiveOWrites { self: OWrites.type =>
  final def recursive[A](f: OWrites[A] ?=> OWrites[A]): OWrites[A] = {
    lazy val res: OWrites[A] = f(using RecursiveOWrites.DeferredOWrites(() => res))
    res
  }
}

private[json] object RecursiveOWrites {
  private final case class DeferredOWrites[A](value: () => OWrites[A]) extends OWrites[A] {
    private lazy val resolved: OWrites[A] = resolve(value)

    @tailrec
    private def resolve(f: () => OWrites[A]): OWrites[A] =
      f() match {
        case DeferredOWrites(f) =>
          resolve(f)
        case next =>
          next
      }

    override def writes(o: A): JsObject =
      resolved.writes(o)
  }
}
