/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import scala.annotation.{ meta, StaticAnnotation }

/**
 * Field annotations for JSON macros (`Json.reads` / `Json.writes` / `Json.format`).
 *
 * ==Usage limitations==
 *
 *  - Annotations apply to '''case class constructor parameters''' processed by the macros.
 *  - `@Ignore` requires a way to materialize the value on read: a constructor default,
 *    or an `Option` field (read as `None`). Constructor defaults are used for `@Ignore`
 *    even when `Json.DefaultValues` is not enabled.
 *  - `@Ignore` / `@transient` fields are omitted from JSON on write.
 *  - `@Flatten` applies to a nested type that is itself represented as a JSON '''object'''.
 *    On write an `OWrites` (or `OFormat`) for the nested type is required at compile time so
 *    the nested value is known to be a `JsObject`. Nested primitives, arrays, or plain
 *    `Writes` that only produce non-object values are rejected when generating writers.
 *  - `@Flatten` on a recursive self-type is rejected at compile time.
 *  - `@Flatten Option[T]`: on write, `Some` merges nested object fields into the parent and
 *    `None` omits them; on read, a successful nested object read becomes `Some`, otherwise
 *    `None` (nested validation errors are not distinguished from absence — prefer non-`Option`
 *    flatten when strict validation is required).
 *  - Overlapping JSON keys between a parent field and a flattened nested object are not
 *    rejected at compile time in this MVP; later fields may overwrite earlier ones on write.
 *  - Per-field `@Key` / `@DefaultValue` annotations are out of scope for this MVP
 *    (use `JsonConfiguration.naming` and `Json.DefaultValues` / constructor defaults).
 */
private[json] trait JsonAnnotations { json: Json.type =>

  /** Annotations to use on case classes that are being processed by macros. */
  object Annotations {

    /**
     * Indicates that the annotated field must not be serialized to JSON.
     * Annotation `@transient` can also be used to achieve the same purpose.
     *
     * If the annotated field must be read, a default value must be available
     * (constructor default), or the field type must be `Option[_]` (read as `None`).
     *
     * {{{
     * import play.api.libs.json.Json
     * import play.api.libs.json.Json.Annotations.Ignore
     *
     * case class User(
     *   name: String,
     *   @Ignore passwordHash: String = ""
     * )
     *
     * // JSON: { "name": "alice" }
     * }}}
     */
    @meta.param // Scala 2
    @meta.field
    final class Ignore extends StaticAnnotation {
      override def hashCode: Int = 1278101060

      override def equals(that: Any): Boolean = that match {
        case _: this.type => true
        case _            => false
      }
    }

    /**
     * Indicates that if a property is represented as a JSON object itself,
     * the object fields are directly included in the parent object,
     * rather than nesting it under the property name.
     *
     * {{{
     * import play.api.libs.json.Json
     * import play.api.libs.json.Json.Annotations.Flatten
     *
     * case class Range(start: Int, end: Int)
     *
     * case class LabelledRange(
     *   name: String,
     *   @Flatten range: Range
     * )
     *
     * // JSON: { "name": "foo", "start": 0, "end": 1 }
     * // rather than: { "name": "foo", "range": { "start": 0, "end": 1 } }
     * }}}
     *
     * See [[JsonAnnotations]] for limitations (`Option`, non-object nested types, collisions).
     */
    @meta.param // Scala 2
    @meta.field
    final class Flatten extends StaticAnnotation {
      override def hashCode: Int = 488571557

      override def equals(that: Any): Boolean = that match {
        case _: this.type => true
        case _            => false
      }
    }
  }
}
