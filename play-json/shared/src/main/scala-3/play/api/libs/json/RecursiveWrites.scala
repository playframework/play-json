/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import scala.annotation.tailrec

trait RecursiveWrites { self: Writes.type =>
  final def recursive[A](f: Writes[A] ?=> Writes[A]): Writes[A] = {
    lazy val res: Writes[A] = f(using RecursiveWrites.DeferredWrites(() => res))
    res
  }
}

private[json] object RecursiveWrites {
  private final case class DeferredWrites[A](value: () => Writes[A]) extends Writes[A] {
    private lazy val resolved: Writes[A] = resolve(value)

    @tailrec
    private def resolve(f: () => Writes[A]): Writes[A] =
      f() match {
        case DeferredWrites(f) =>
          resolve(f)
        case next =>
          next
      }

    override def writes(o: A): JsValue =
      resolved.writes(o)
  }
}
