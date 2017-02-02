/*
 * Copyright (C) 2009-2016 Lightbend Inc. <https://www.lightbend.com>
 */
package play.api.libs.json

import play.api.libs.functional.Functor

import JsResult.functorJsResult

import org.scalatest._

class JsResultSpec extends WordSpec with MustMatchers {
  "JSON Result" should {
    "be functor" in {
      val jsres = JsSuccess("jsStr")

      implicitly[Functor[JsResult]].
        fmap[String, List[Char]](jsres, _.toList).mustEqual(
          JsSuccess(List('j', 's', 'S', 't', 'r'))
        )
    }
  }
}
