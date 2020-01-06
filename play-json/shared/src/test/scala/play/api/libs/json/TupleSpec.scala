/*
 * Copyright (C) Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import org.scalatest._
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

final class TupleSpec extends AnyWordSpec with Matchers {
  "Reading/Write tuples" should {
    def check[T: Reads: Writes](value: T, expected: String) = {
      Json.stringify(Json.toJson(value)).mustEqual(expected)
      Json.fromJson(Json.parse(expected)).get.mustEqual(value)
    }

    "work for small tuples" in {
      check(Tuple1(1), "[1]")
      check((1, 2, "lol"), """[1,2,"lol"]""")
    }

    "work for large tuples" in {
      check(
        (1, 2, "lol", "foo", "bar", "baz", true, Seq(1, 2)),
        """[1,2,"lol","foo","bar","baz",true,[1,2]]"""
      )
    }

    "work for nested tuples" in {
      check(
        (1, 2, ("lol", ("foo", "bar"))),
        """[1,2,["lol",["foo","bar"]]]"""
      )
    }
  }
}
