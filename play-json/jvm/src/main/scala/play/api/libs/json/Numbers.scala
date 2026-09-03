/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import com.fasterxml.jackson.core.io.{ NumberInput, NumberOutput }

private[json] object Numbers {
  val parseLong: String => Long = NumberInput.parseLong(_: String)

  val isValidLong: String => Boolean = { text =>
    if (text.charAt(0) == '-') NumberInput.inLongRange(text.drop(1), true)
    else NumberInput.inLongRange(text, false)
  }

  def formatDouble(d: Double): String = NumberOutput.toString(d)

  def formatInt(i: Int): String = NumberOutput.toString(i)

  def formatLong(l: Long): String = NumberOutput.toString(l)
}
