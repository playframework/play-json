/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import scala.annotation.tailrec

trait RecursiveOWrites { self: OWrites.type =>

  /**
   * Constructs an `OWrites` for a recursive type.
   *
   * While `f` is evaluated, a deferred `OWrites[A]` is available as a
   * contextual value. This lets object writers derived inside `f` refer to
   * the resulting writer without forcing it during initialization.
   *
   * This method is available only in Scala 3.
   *
   * @tparam A the type written as a JSON object
   * @param f function that constructs the recursive object writer
   */
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
