/*
 * Copyright (C) 2009-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import scala.language.experimental.macros

import org.scalactic.source._

// Remove once there is a scalatest_sjs1_3.0.0_M3 (or later) available.
object ScalaTestPosition {
  implicit def here: Position = macro PositionMacro.genPosition
}
