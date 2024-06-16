/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import org.openjdk.jol.info.GraphLayout
import org.scalatest.freespec.AnyFreeSpec
import scala.util.Properties
import scala.util.chaining._

class JsonMemoryFootprintSpec extends AnyFreeSpec {

  "Json.parse" - {
    "obj0" in assertSizes("""{}""", 16, 16)
    "obj1" in assertSizes("""{"1":true}""", 152, 168, expectedJdk21 = Some(160), hashedJdk21 = Some(184))
    "obj4" in assertSizes(
      """{"1":true,"2":true,"3":true,"4":true}""",
      296,
      312,
      expectedJdk21 = Some(304),
      hashedJdk21 = Some(328)
    )

    "arr0" in assertSizes("""[]""", 40, 40)
    "arr1" in assertSizes("""[true]""", 120, 120)
    "arr4" in assertSizes("""[true,true,true,true]""", 120, 120)

    "num0" in assertSizes("""0""", 80, 80)
    "num0.1" in assertSizes("""0.1""", 80, 80)
    "num0.5" in assertSizes("""0.5""", 80, 80)
    "numLongMax" in assertSizes(Long.MaxValue.toString, 144, 144)
    "numDoubleMax" in assertSizes(Double.MaxValue.toString, 144, 144)

    "true" in assertSizes("""true""", 0, 0)
    "false" in assertSizes("""false""", 0, 0)
    "null" in assertSizes("""null""", 0, 0)
  }

  "JsObject" - {
    def obj(json: String) = Json.parse(json).as[JsObject]
    "obj0 ++ obj0" in assertSize(obj("{}") ++ obj("{}"), 16)
    "obj0 ++ obj1" in assertSize(obj("{}") ++ obj("""{"1":true}"""), 152, expectedJdk21 = Some(160))
    "obj1 ++ obj0" in assertSize(obj("""{"1":true}""") ++ obj("""{}"""), 152, expectedJdk21 = Some(160))

    "obj1.value" in assertSize(obj("""{"1":true}""").tap(_.value), 152, expectedJdk21 = Some(160))
  }

  "malicious" - {
    // if we pack data into ~1KB of input, how much memory amplification can we achieve?
    def arr1KB(elem: String, targetSize: Int = 1000): String =
      Iterator.continually(elem).take(targetSize / (elem.length + 1)).mkString("[", ",", "]")
    "obj0" in assertSizes(arr1KB("{}"), 7432, 7432)
    "obj1" in assertSizes(arr1KB("""{"a":6}"""), 29568, 31568, expectedJdk21 = Some(30568), hashedJdk21 = Some(33568))
    "nums" in assertSizes(arr1KB("6"), 42104, 42104)
    "arr0" in assertSizes(arr1KB("[]"), 15424, 15424)
    "arr1" in assertSizes(arr1KB("[6]"), 51080, 51080)
  }

  private def assertSizes(
      input: String,
      expected: Long,
      hashed: Long,
      expectedJdk21: Option[Long] = None,
      hashedJdk21: Option[Long] = None
  ) = {
    assertSize(Json.parse(input), expected, expectedJdk21)
    withClue("After hashCode():")(
      assertSize(
        {
          val t = Json.parse(input)
          t.hashCode()
          t
        },
        hashed,
        hashedJdk21
      )
    )
  }

  private def assertSize(a: => JsValue, expected: Long, expectedJdk21: Option[Long] = None) = {
    val layout1  = GraphLayout.parseInstance(a)
    val layout2  = GraphLayout.parseInstance(a)
    val distinct = layout1.subtract(layout2) // shared singletons don't count.
    val clue =
      s"""$a:
         |${distinct.toFootprint}
         |${distinct.toPrintable}""".stripMargin

    withClue(clue) {
      assert(distinct.totalSize() === expectedJdk21.filter(_ => Properties.isJavaAtLeast("21")).getOrElse(expected))
    }
  }
}
