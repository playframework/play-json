/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

// Scala 3.10 no longer accepts `with` as an intersection-type operator. Keep this alias in
// version-specific sources because Scala 2 requires `with`, whereas Scala 3 uses `&`.
/* Scala compatibility trait for the `Json` companion */
private[json] trait ScalaCompatJson { self: Json.type =>

  /**
   * Alias for `MacroOptions & DefaultValues`.
   *
   * {{{
   * import play.api.libs.json.Json
   *
   * Json.using[Json.WithDefaultValues]
   * }}}
   */
  type WithDefaultValues = MacroOptions & DefaultValues
}
