/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

/* Scala compatibility trait for the `Writes` companion */
private[json] trait ScalaCompatWrites { self: Writes.type => }

/* Scala compatibility trait for the `OWrites` companion */
private[json] trait ScalaCompatOWrites { self: OWrites.type => }
