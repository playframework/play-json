/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

final class FormatSharedSpec extends AnyWordSpec with Matchers {
  "Format" should {
    "be bimap'ed" in {
      val strFormat              = implicitly[Format[String]]
      val intFormat: Format[Int] =
        strFormat.bimap(_.size, List.fill(_: Int)('X').mkString)

      intFormat.reads(JsString("foo")).mustEqual(JsSuccess(3))
      intFormat.writes(5).mustEqual(JsString("XXXXX"))
    }
  }

  "OFormat" should {
    "be bimap'ed" in {
      val fooFormat                  = Json.format[Foo]
      val strFormat: OFormat[String] = fooFormat.bimap(_.name, Foo(_))

      val expectedRepr = Json.obj("name" -> "bar")

      strFormat.reads(expectedRepr).mustEqual(JsSuccess("bar"))
      strFormat.writes("bar").mustEqual(expectedRepr)
    }
  }

  case class Foo(name: String)
}
