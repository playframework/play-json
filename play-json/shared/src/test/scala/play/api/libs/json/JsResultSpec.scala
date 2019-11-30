/*
 * Copyright (C) 2009-2019 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import scala.util.Failure
import scala.util.Success

import play.api.libs.functional.Functor

import JsResult.functorJsResult

import org.scalatest._
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class JsResultSpec extends AnyWordSpec with Matchers {
  "JSON Result" should {
    "be functor" in {
      val jsres = JsSuccess("jsStr")

      implicitly[Functor[JsResult]]
        .fmap[String, List[Char]](jsres, _.toList)
        .mustEqual(
          JsSuccess(List('j', 's', 'S', 't', 'r'))
        )
    }

    "be converted to Success" in {
      JsResult.toTry(JsSuccess("foo")).mustEqual(Success("foo"))
    }

    "be converted to basic Failure" in {
      val err = JsError("bar")
      JsResult.toTry(err).mustEqual(Failure(JsResult.Exception(err)))
    }

    "be created from a Success" in {
      JsResult.fromTry(Success("foo")).mustEqual(JsSuccess("foo"))
    }

    "be created from a Failure" in {
      JsResult.fromTry(Failure(new Throwable("bar"))).mustEqual(JsError("bar"))
    }
  }

  "JsSuccess" should {
    "be recovered" in {
      val succ = JsSuccess("success")

      succ
        .recoverWith { _ =>
          JsSuccess("fallback")
        }
        .mustEqual(succ)
    }
  }

  "JsError" should {
    "be recovered" in {
      val succ = JsSuccess("success")

      JsError("err")
        .recoverWith { _ =>
          succ
        }
        .mustEqual(succ)
    }
  }

  "JsResult" should {
    "return true for JsSuccess(x)#contains(x)" in {
      assert(JsSuccess(1).contains(1))
    }

    "return false for JsSuccess(x)#contains(y)" in {
      assert(!JsSuccess(1).contains(2))
    }

    "return false for JsError(_)#contains(_)" in {
      assert(!JsError().contains(1))
    }

    "return true for JsSuccess(x)#exists(p) if p(x) == true" in {
      assert(JsSuccess(1).exists(_ == 1))
    }

    "return false for JsSuccess(x)#exists(p) if p(x) == false" in {
      assert(!JsSuccess(1).exists(_ == 2))
    }

    "return false for JsError(_).exists(_)" in {
      assert(!JsError().exists(_ == 1))
    }
  }

  "JsSuccess#forall" should {
    "return true for JsSuccess(x * 2).forall(_ % 2 == 0)" in {
      assert(JsSuccess(2).forall(_ % 2 == 0))
    }

    "return false for JsSuccess(x, {x < 0}).forall(_ >)" in {
      assert(!JsSuccess(-1).forall(_ > 0))
    }
  }

  "JsError#forall" should {
    "return true" in {
      assert(JsError("").forall(_ => false))
    }
  }
}
