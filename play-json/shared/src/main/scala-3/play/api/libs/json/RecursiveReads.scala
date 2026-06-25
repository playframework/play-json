/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import scala.annotation.tailrec

trait RecursiveReads { self: Reads.type =>
  final def recursive[A](f: Reads[A] ?=> Reads[A]): Reads[A] = {
    lazy val res: Reads[A] = f(using RecursiveReads.DeferredReads(() => res))
    res
  }
}

private[json] object RecursiveReads {
  private final case class DeferredReads[A](value: () => Reads[A]) extends Reads[A] {
    private lazy val resolved: Reads[A] = resolve(value)

    @tailrec
    private def resolve(f: () => Reads[A]): Reads[A] =
      f() match {
        case DeferredReads(f) =>
          resolve(f)
        case next =>
          next
      }

    override def reads(json: JsValue): JsResult[A] =
      resolved.reads(json)
  }
}
