/*
 * Copyright (C) 2009-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import scala.language.experimental.macros

// Replace with ScalaTest's `shouldNot typeCheck` once there is a scalatest_sjs1_3.0.0_M3 (or later) available.
/** A utility which ensures that a code fragment does not typecheck. */
object illTyped {
  def apply(code: String): Unit = macro shapeless.test.IllTypedMacros.applyImplNoExp
  def apply(code: String, expected: String): Unit = macro shapeless.test.IllTypedMacros.applyImpl
}
