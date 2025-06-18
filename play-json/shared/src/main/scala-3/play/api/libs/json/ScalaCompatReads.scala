/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

/* Scala compatibility trait for the `Reads` companion */
private[json] trait ScalaCompatReads { self: Reads.type =>

  /**
   * Constructs a `Reads` for a recursive type.
   *
   * While `f` is evaluated, a deferred `Reads[A]` is available as a
   * contextual value. This lets readers derived inside `f` refer to the
   * resulting reader without forcing it during initialization.
   *
   * This method is available only in Scala 3.
   *
   * @tparam A the type read from JSON
   * @param f function that constructs the recursive reader
   */
  final def recursive[A](f: Reads[A] ?=> Reads[A]): Reads[A] = {
    lazy val res: Reads[A] = f(using ScalaCompatReads.DeferredReads(() => res))
    res
  }

  /**
   * Constructs a `Reads` for a type using its derived JSON reader.
   *
   * This method delegates to `Json.reads` to derive a `Reads[T]` for the specified type.
   *
   * This method is available only in Scala 3.
   *
   * @tparam T the type read from JSON
   * @return a derived reader for T
   */
  inline def derived[T]: Reads[T] = Json.reads[T]
}

private[json] object ScalaCompatReads {
  private final case class DeferredReads[A](value: () => Reads[A]) extends Reads[A] {
    private lazy val resolved: Reads[A] = resolve(value)

    @annotation.tailrec
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
