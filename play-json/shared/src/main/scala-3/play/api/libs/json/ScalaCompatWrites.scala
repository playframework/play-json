/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

/* Scala compatibility trait for the `Writes` companion */
private[json] trait ScalaCompatWrites { self: Writes.type =>

  /**
   * Constructs a `Writes` for a recursive type.
   *
   * While `f` is evaluated, a deferred `Writes[A]` is available as a
   * contextual value. This lets writers derived inside `f` refer to the
   * resulting writer without forcing it during initialization.
   *
   * This method is available only in Scala 3.
   *
   * @tparam A the type written as JSON
   * @param f function that constructs the recursive writer
   */
  final def recursive[A](f: Writes[A] ?=> Writes[A]): Writes[A] = {
    lazy val res: Writes[A] = f(using ScalaCompatWrites.DeferredWrites(() => res))
    res
  }
}

private[json] object ScalaCompatWrites {
  private final case class DeferredWrites[A](value: () => Writes[A]) extends Writes[A] {
    private lazy val resolved: Writes[A] = resolve(value)

    @annotation.tailrec
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

/* Scala compatibility trait for the `OWrites` companion */
private[json] trait ScalaCompatOWrites { self: OWrites.type =>

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
    lazy val res: OWrites[A] = f(using ScalaCompatOWrites.DeferredOWrites(() => res))
    res
  }

  /**
   * Constructs an `OWrites` for a type using its derived JSON writer.
   *
   * This method delegates to `Json.writes` to derive an `OWrites[T]`
   * for the specified type.
   *
   * This method is available only in Scala 3.
   *
   * @tparam T the type written as JSON
   * @return a derived writer for T
   */
  inline def derived[T]: OWrites[T] = Json.writes[T]
}

private[json] object ScalaCompatOWrites {
  private final case class DeferredOWrites[A](value: () => OWrites[A]) extends OWrites[A] {
    private lazy val resolved: OWrites[A] = resolve(value)

    @annotation.tailrec
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
