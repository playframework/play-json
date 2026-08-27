/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

private[json] trait ScalaCompatFormat { self: Format.type =>

  /**
   * Constructs a `Format` for a recursive type.
   *
   * While `f` is evaluated, a deferred `Format[A]` is available as a
   * contextual value. This lets formats derived inside `f` refer to the
   * resulting format without forcing it during initialization.
   *
   * This method is available only in Scala 3.
   *
   * @tparam A the type read from and written as JSON
   * @param f function that constructs the recursive format
   */
  final def recursive[A](f: Format[A] ?=> Format[A]): Format[A] = {
    lazy val res: Format[A] = f(using ScalaCompatFormat.DeferredFormat(() => res))
    res
  }
}

private[json] object ScalaCompatFormat {
  private final case class DeferredFormat[A](value: () => Format[A]) extends Format[A] {
    private lazy val resolved: Format[A] = resolve(value)

    @annotation.tailrec
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

private[json] trait ScalaCompatOFormat { self: OFormat.type =>

  /**
   * Constructs an `OFormat` for a recursive type.
   *
   * While `f` is evaluated, a deferred `OFormat[A]` is available as a
   * contextual value. This lets object formats derived inside `f` refer to
   * the resulting format without forcing it during initialization.
   *
   * This method is available only in Scala 3.
   *
   * @tparam A the type read from and written as a JSON object
   * @param f function that constructs the recursive object format
   */
  final def recursive[A](f: OFormat[A] ?=> OFormat[A]): OFormat[A] = {
    lazy val res: OFormat[A] = f(using ScalaCompatOFormat.DeferredOFormat(() => res))
    res
  }
}

private[json] object ScalaCompatOFormat {
  private final case class DeferredOFormat[A](value: () => OFormat[A]) extends OFormat[A] {
    private lazy val resolved: OFormat[A] = resolve(value)

    @annotation.tailrec
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
