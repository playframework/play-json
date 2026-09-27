/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import scala.util.control.NonFatal

private[json] object Numbers {
  def parseLong(text: String): Long = text.toLong

  def isValidLong(text: String): Boolean = try {
    text.toLong.toString == text
  } catch {
    case NonFatal(_) =>
      false
  }

  def formatInt(i: Int): String = i.toString

  def formatLong(l: Long): String = l.toString

  def formatDouble(d: Double): String = d.toString
}
