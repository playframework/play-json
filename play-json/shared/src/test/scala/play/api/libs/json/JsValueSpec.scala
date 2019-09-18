/*
 * Copyright (C) 2009-2019 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import play.api.libs.json.Json._

import org.scalatest._

final class JsValueSpec extends WordSpec with MustMatchers {
  "JsNull" should {
    "equal JsNumber(null)" in {
      val num = JsNumber(null)

      JsNull mustEqual num
      num mustEqual JsNull
    }

    "equal JsString(null)" in {
      val str = JsString(null)

      JsNull mustEqual str
      str mustEqual JsNull
    }

    "equal JsArray(null)" in {
      val arr = JsArray(null)

      JsNull mustEqual arr
      arr mustEqual JsNull
    }

    "equal JsObject(null)" in {
      val obj = JsObject(null.asInstanceOf[Map[String, JsValue]])

      JsNull mustEqual obj
      obj mustEqual JsNull
    }
  }
}
