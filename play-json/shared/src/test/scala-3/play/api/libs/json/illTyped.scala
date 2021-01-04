/*
 * Copyright (C) 2009-2020 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import scala.compiletime.testing._

// Replace with ScalaTest's `shouldNot typeCheck` once there is a scalatest_sjs1_3.0.0_M3 (or later) available.
/** A utility which ensures that a code fragment does not typecheck. */
object illTyped {
  inline def apply(code: String): Unit                   = assert(!typeChecks(code))
  inline def apply(code: String, expected: String): Unit = apply(code)
}
