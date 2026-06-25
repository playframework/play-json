/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import scala.annotation.tailrec

trait RecursiveOFormat { self: OFormat.type =>
  final def recursive[A](f: OFormat[A] ?=> OFormat[A]): OFormat[A] = {
    lazy val res: OFormat[A] = f(using RecursiveOFormat.DeferredOFormat(() => res))
    res
  }
}

private[json] object RecursiveOFormat {
  private final case class DeferredOFormat[A](value: () => OFormat[A]) extends OFormat[A] {
    private lazy val resolved: OFormat[A] = resolve(value)

    @tailrec
    private def resolve(f: () => OFormat[A]): OFormat[A] =
      f() match {
        case DeferredOFormat(f) =>
          resolve(f)
        case next =>
          next
      }

    override def reads(json: JsValue): JsResult[A] =
      resolved.reads(json)

    override def writes(o: A): JsObject =
      resolved.writes(o)
  }
}
