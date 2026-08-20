/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import scala.language.experimental.macros

import java.lang.{ Enum => JEnum }

private[json] trait EnvValueMacros { _: JsValueMacros =>
  def javaEnumReads[T <: JEnum[?]]: Reads[T] =
    macro JavaEnumHandlerImpl.reads[T]

  def javaEnumFormat[T <: JEnum[?]]: Format[T] =
    macro JavaEnumHandlerImpl.format[T]
}
