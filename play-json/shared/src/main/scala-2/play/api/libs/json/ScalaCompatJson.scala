/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

/* Scala compatibility trait for the `Json` companion */
private[json] trait ScalaCompatJson { self: Json.type =>

  /**
   * Alias for `MacroOptions with DefaultValues`.
   *
   * {{{
   * import play.api.libs.json.Json
   *
   * Json.using[Json.WithDefaultValues]
   * }}}
   */
  type WithDefaultValues = MacroOptions with DefaultValues
}
