/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import scala.annotation.tailrec

trait RecursiveFormat { self: Format.type =>
  final def recursive[A](f: Format[A] ?=> Format[A]): Format[A] = {
    lazy val res: Format[A] = f(using RecursiveFormat.DeferredFormat(() => res))
    res
  }
}

private[json] object RecursiveFormat {
  private final case class DeferredFormat[A](value: () => Format[A]) extends Format[A] {
    private lazy val resolved: Format[A] = resolve(value)

    @tailrec
    private def resolve(f: () => Format[A]): Format[A] =
      f() match {
        case DeferredFormat(f) =>
          resolve(f)
        case next =>
          next
      }

    override def reads(json: JsValue): JsResult[A] =
      resolved.reads(json)

    override def writes(o: A): JsValue =
      resolved.writes(o)
  }
}
