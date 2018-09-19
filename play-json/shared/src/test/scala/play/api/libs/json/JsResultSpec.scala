/*
 * Copyright (C) 2009-2018 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import scala.util.{ Failure, Success }

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

    "be converted to Success" in {
      JsResult.toTry(JsSuccess("foo")) mustEqual Success("foo")
    }

    "be converted to basic Failure" in {
      val err = JsError("bar")
      JsResult.toTry(err) mustEqual Failure(JsResult.Exception(err))
    }
  }

  "JsSuccess" should {
    "be recovered" in {
      val succ = JsSuccess("success")

      succ.recoverWith { _ => JsSuccess("fallback") } mustEqual succ
    }
  }

  "JsError" should {
    "be recovered" in {
      val succ = JsSuccess("success")

      JsError("err").recoverWith { _ => succ } mustEqual succ
    }
  }

  "JsResult" should {

    "return true for JsSuccess(x)#contains(x)" in {
      val x = 10
      assert(JsSuccess(x).contains(10))
    }

    "return false for JsSuccess(x)#contains(y)" in {
      val x = 10
      assert(!JsSuccess(x).contains(x * 2))
    }

    "return false for JsError(_)#contains(_)" in {
      assert(!JsError().contains(10))
    }

    "return true for JsSuccess(x)#exists(p) if p(x) == true" in {
      val x = 10
      assert(JsSuccess(x).exists(_ == x))
    }

    "return false for JsSuccess(x)#exists(p) if p(x) == false" in {
      val x = 10
      assert(JsSuccess(x).exists(_ == x))
    }

    "return false for JsError(_).exists(_)" in {
      assert(!JsError().exists(_ == 10))
    }

  }

}
