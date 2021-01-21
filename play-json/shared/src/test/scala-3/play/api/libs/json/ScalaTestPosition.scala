/*
 * Copyright (C) 2009-2020 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import org.scalactic.source._

// Remove once there is a scalatest_sjs1_3.0.0_M3 (or later) available.
object ScalaTestPosition {
  implicit def here: Position = Position("", "", 0)
}
