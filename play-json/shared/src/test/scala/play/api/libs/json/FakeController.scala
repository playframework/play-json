/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.mvc // Allow compile scaladoc without Play dependency

import play.api.libs.json.JsValue

trait Controller {
  def Ok(r: JsValue)     = ???
  def Ok                 = ???
  def Action[T](f: => T) = ???
}
