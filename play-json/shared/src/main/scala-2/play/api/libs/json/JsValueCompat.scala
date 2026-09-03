/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

private[json] trait JsValueCompat { _: JsValue.type => }

private[json] trait JsNumberCompat { _: JsNumber.type => }
