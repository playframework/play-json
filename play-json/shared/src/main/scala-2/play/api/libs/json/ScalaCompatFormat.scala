/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

/* Scala compatibility trait for the `Format` companion */
private[json] trait ScalaCompatFormat { self: Format.type => }

/* Scala compatibility trait for the `OFormat` companion */
private[json] trait ScalaCompatOFormat { self: OFormat.type => }
