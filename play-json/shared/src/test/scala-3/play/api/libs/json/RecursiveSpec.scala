/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import org.scalatest.wordspec.AnyWordSpec
import play.api.libs.json.RecursiveSpec.Foo
import play.api.libs.functional.syntax.*

final class RecursiveSpec extends AnyWordSpec {
  "recursive" should {
    "Reads" in {
      given Reads[Foo] = Reads.recursive(
        (
          (__ \ "x").read[Int] and
            (__ \ "y").read[List[Foo]]
        )(Foo.apply)
      )
      assert(Foo.json1.as[Foo] == Foo.value1)
    }
    "Writes" in {
      given Writes[Foo] = Writes.recursive(
        (
          (__ \ "x").write[Int] and
            (__ \ "y").write[List[Foo]]
        )(Tuple.fromProductTyped(_))
      )
      assert(Json.toJson(Foo.value1) == Foo.json1)
    }
    "OWrites" in {
      given OWrites[Foo] = OWrites.recursive(
        (
          (__ \ "x").write[Int] and
            (__ \ "y").write[List[Foo]]
        )(Tuple.fromProductTyped(_))
      )
      assert(Json.toJson(Foo.value1) == Foo.json1)
    }
    "Format" in {
      given Format[Foo] = Format.recursive(
        (
          (__ \ "x").format[Int] and
            (__ \ "y").format[List[Foo]]
        )(Foo.apply, Tuple.fromProductTyped)
      )
      assert(Json.toJson(Foo.value1) == Foo.json1)
      assert(Foo.json1.as[Foo] == Foo.value1)
    }
    "OFormat" in {
      given OFormat[Foo] = OFormat.recursive(
        (
          (__ \ "x").format[Int] and
            (__ \ "y").format[List[Foo]]
        )(Foo.apply, Tuple.fromProductTyped)
      )
      assert(Json.toJson(Foo.value1) == Foo.json1)
      assert(Foo.json1.as[Foo] == Foo.value1)
    }
  }
}

object RecursiveSpec {
  private final case class Foo(x: Int, y: List[Foo])

  private object Foo {
    val value1: Foo = Foo(
      1,
      List(
        Foo(2, Nil),
        Foo(3, List(Foo(4, Nil))),
        Foo(5, Nil),
        Foo(
          6,
          List(
            Foo(7, Nil),
            Foo(8, Nil)
          )
        ),
      )
    )

    val json1: JsObject = Json.obj(
      "x" -> 1,
      "y" -> Json.arr(
        Json.obj(
          "x" -> 2,
          "y" -> Json.arr()
        ),
        Json.obj(
          "x" -> 3,
          "y" -> Json.arr(
            Json.obj(
              "x" -> 4,
              "y" -> Json.arr(),
            )
          )
        ),
        Json.obj(
          "x" -> 5,
          "y" -> Json.arr()
        ),
        Json.obj(
          "x" -> 6,
          "y" -> Json.arr(
            Json.obj(
              "x" -> 7,
              "y" -> Json.arr(),
            ),
            Json.obj(
              "x" -> 8,
              "y" -> Json.arr(),
            ),
          )
        )
      )
    )
  }
}
