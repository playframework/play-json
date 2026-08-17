/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import org.scalacheck.Gen
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MacroScala2Spec extends AnyWordSpec with Matchers with org.scalatestplus.scalacheck.ScalaCheckPropertyChecks {
  import MacroScala2Spec._

  "Macro" should {
    // lampepfl/dotty-feature-requests#163 No Mirrors in Scala 3 for case classes with implicits
    "handle case class with implicits" when {
      val json1    = Json.obj("pos" -> 2, "text" -> "str")
      val json2    = Json.obj("ident" -> "id", "value" -> 23.456D)
      val fixture1 = WithImplicit1(2, "str")
      val fixture2 = WithImplicit2("id", 23.456D)

      def readSpec1(r: Reads[WithImplicit1]) =
        r.reads(json1).mustEqual(JsSuccess(fixture1))

      def writeSpec2(w: OWrites[WithImplicit2[Double]]) =
        w.writes(fixture2).mustEqual(json2)

      "to generate Reads" in readSpec1(Json.reads[WithImplicit1])

      "to generate Writes with type parameters" in writeSpec2(
        Json.writes[WithImplicit2[Double]]
      )

      "to generate Format" in {
        val f1 = Json.format[WithImplicit1]
        val f2 = Json.format[WithImplicit2[Double]]

        readSpec1(f1)
        f1.writes(fixture1).mustEqual(json1)
        writeSpec2(f2)
        f2.reads(json2).mustEqual(JsSuccess(fixture2))
      }
    }

    // lampepfl/dotty#7000 No Mirrors for value classes
    "handle ValueClass" in {
      val id                           = new TextId("foo")
      val js                           = JsString("foo")
      implicit val fmt: Format[TextId] = Json.valueFormat[TextId]

      js.validate[TextId].mustEqual(JsSuccess(id))
      fmt.writes(id).mustEqual(js)
    }
  }

  // playframework/play-json#3 Case class with >22 field not supported in Scala2 Macros
  private def pads(n: Int): JsObject = JsObject(1.to(n).map(i => s"pad$i" -> JsNumber(i)))

  "Reads for classes with more than 22 fields" should {
    "be generated for simple case class" in {
      val json     = Json.obj("bar" -> "lorem") ++ pads(22)
      val expected = BigSimple("lorem", 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22)

      forAll(
        Json.reads[BigSimple],
      ) { _.reads(json).mustEqual(JsSuccess(expected)) }
    }

    "refuse value other than JsObject when properties are optional" in {
      forAll(Gen.oneOf(Json.reads[BigOptional], Json.format[BigOptional])) { r =>
        r.reads(Json.obj())
          .mustEqual(
            JsSuccess(
              BigOptional(
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None
              )
            )
          )

        (r.reads(JsString("foo")).asEither match {
          case Left((_, err :: Nil) :: Nil) =>
            err.message == "error.expected.jsobject"

          case _ => false
        }).mustEqual(true)
      }
    }
  }

  "Writes for classes with more than 22 fields" should {
    "be generated for simple case class" in {
      Json
        .writes[BigSimple]
        .writes(BigSimple("lorem", 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22))
        .mustEqual(Json.obj("bar" -> "lorem") ++ pads(22))
    }
  }

  "Macro for classes with more than 22 fields" should {
    "handle case class with self type as nested type parameter" when {
      import TestFormats._
      val jsonNoValue  = Json.obj("id" -> "A") ++ pads(21)
      val jsonStrValue = Json.obj("id" -> "B", "value" -> "str") ++ pads(21)
      val jsonFooValue = Json.obj("id" -> "C", "value" -> jsonStrValue) ++ pads(21)

      val fooStrValue =
        BigFoo(
          BigFoo.id("B"),
          Some(Left("str")),
          1,
          2,
          3,
          4,
          5,
          6,
          7,
          8,
          9,
          10,
          11,
          12,
          13,
          14,
          15,
          16,
          17,
          18,
          19,
          20,
          21
        )
      val fooFooValue = BigFoo(
        BigFoo.id("C"),
        Some(Right(fooStrValue)),
        1,
        2,
        3,
        4,
        5,
        6,
        7,
        8,
        9,
        10,
        11,
        12,
        13,
        14,
        15,
        16,
        17,
        18,
        19,
        20,
        21
      )

      def readSpec(r: Reads[BigFoo]) = {
        r.reads(jsonNoValue)
          .mustEqual(
            JsSuccess(
              BigFoo(BigFoo.id("A"), None, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21)
            )
          )
        r.reads(jsonStrValue).mustEqual(JsSuccess(fooStrValue))
        r.reads(jsonFooValue).mustEqual(JsSuccess(fooFooValue))
        r.reads(Json.obj("id" -> "D", "value" -> jsonFooValue) ++ pads(21))
          .mustEqual(
            JsSuccess(
              BigFoo(
                BigFoo.id("D"),
                Some(Right(fooFooValue)),
                1,
                2,
                3,
                4,
                5,
                6,
                7,
                8,
                9,
                10,
                11,
                12,
                13,
                14,
                15,
                16,
                17,
                18,
                19,
                20,
                21
              )
            )
          )
      }

      def writeSpec(w: Writes[BigFoo]) = {
        w.writes(
          BigFoo(BigFoo.id("A"), None, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21)
        ).mustEqual(jsonNoValue)
        w.writes(fooStrValue).mustEqual(jsonStrValue)
        w.writes(fooFooValue).mustEqual(jsonFooValue)
        w.writes(
          BigFoo(
            BigFoo.id("D"),
            Some(Right(fooFooValue)),
            1,
            2,
            3,
            4,
            5,
            6,
            7,
            8,
            9,
            10,
            11,
            12,
            13,
            14,
            15,
            16,
            17,
            18,
            19,
            20,
            21
          )
        ).mustEqual(Json.obj("id" -> "D", "value" -> jsonFooValue) ++ pads(21))
      }

      "to generate Reads" in readSpec(Json.reads[BigFoo])

      "to generate Writes" in writeSpec(Json.writes[BigFoo])

      "to generate Format" in {
        val f: OFormat[BigFoo] = Json.format[BigFoo]

        readSpec(f)
        writeSpec(f)
      }
    }

    "handle generic case class with multiple generic parameters" when {
      val jsonNoOther = Json.obj("base" -> 1) ++ pads(21)
      val jsonOther   = Json.obj("base" -> 2, "other" -> 3) ++ pads(21)

      val noOther = BigInterval[Int](1, None, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21)
      val other   =
        BigInterval[Int](2, Some(3), 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21)

      def readSpec(r: Reads[BigInterval[Int]]) = {
        r.reads(jsonNoOther).mustEqual(JsSuccess(noOther))
        r.reads(jsonOther).mustEqual(JsSuccess(other))
      }

      def writeSpec(r: Writes[BigInterval[Int]]) = {
        r.writes(noOther).mustEqual(jsonNoOther)
        r.writes(other).mustEqual(jsonOther)
      }

      "to generate Reads" in readSpec(Json.reads[BigInterval[Int]])

      "to generate Writes" in writeSpec(Json.writes[BigInterval[Int]])

      "to generate Format" in {
        val f = Json.format[BigInterval[Int]]
        readSpec(f)
        writeSpec(f)
      }
    }

    "handle generic case class with multiple generic parameters and self references" when {
      import TestFormats._

      val nestedLeft  = Json.obj("id" -> 2, "a" -> 0.2F, "b" -> 0.3F, "c" -> 3) ++ pads(20)
      val nestedRight = Json.obj("id" -> 1, "a" -> 0.1F, "b" -> "right1", "c" -> 2) ++ pads(20)

      val jsonRight = Json.obj(
        "id" -> 3,
        "a"  -> nestedRight,
        "b"  -> "right2",
        "c"  -> 0.4D
      ) ++ pads(20)

      val jsonLeft = Json.obj(
        "id" -> 4,
        "a"  -> nestedLeft,
        "b"  -> nestedRight,
        "c"  -> 0.5D
      ) ++ pads(20)

      val complexRight = BigComplex(
        3,
        BigComplex(1, 0.1F, Right("right1"), 2, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20),
        Right("right2"),
        0.4D,
        1,
        2,
        3,
        4,
        5,
        6,
        7,
        8,
        9,
        10,
        11,
        12,
        13,
        14,
        15,
        16,
        17,
        18,
        19,
        20
      )

      val complexLeft = BigComplex(
        4,
        BigComplex(2, 0.2F, Left(0.3F), 3, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20),
        Left(
          BigComplex(1, 0.1F, Right("right1"), 2, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)
        ),
        0.5D,
        1,
        2,
        3,
        4,
        5,
        6,
        7,
        8,
        9,
        10,
        11,
        12,
        13,
        14,
        15,
        16,
        17,
        18,
        19,
        20
      )

      def readSpec(r: Reads[BigComplex[BigComplex[Float, Int], Double]]) = {
        r.reads(jsonRight).mustEqual(JsSuccess(complexRight))
        r.reads(jsonLeft).mustEqual(JsSuccess(complexLeft))
      }

      def writeSpec(r: Writes[BigComplex[BigComplex[Float, Int], Double]]) = {
        r.writes(complexRight).mustEqual(jsonRight)
        r.writes(complexLeft).mustEqual(jsonLeft)
      }

      "to generate Reads" in readSpec {
        implicit val nested: Reads[BigComplex[Float, Int]] = Json.reads[BigComplex[Float, Int]]
        Json.reads[BigComplex[BigComplex[Float, Int], Double]]
      }

      "to generate Writes" in writeSpec {
        implicit val nested: OWrites[BigComplex[Float, Int]] = Json.writes[BigComplex[Float, Int]]
        Json.writes[BigComplex[BigComplex[Float, Int], Double]]
      }

      "to generate Format" in {
        implicit val nested: OFormat[BigComplex[Float, Int]] = Json.format[BigComplex[Float, Int]]
        val f                                                = Json.format[BigComplex[BigComplex[Float, Int], Double]]

        readSpec(f)
        writeSpec(f)
      }
    }

    "handle nesting class" in {
      implicit val textIdFormat: Format[TextId] = Json.valueFormat[TextId]

      val nesting = new BigNestingClass

      val expected =
        nesting.Test(
          Some(new TextId("foo")),
          1,
          2,
          3,
          4,
          5,
          6,
          7,
          8,
          9,
          10,
          11,
          12,
          13,
          14,
          15,
          16,
          17,
          18,
          19,
          20,
          21,
          22
        )
      val expectedJson = Json.obj("underlying" -> "foo") ++ pads(22)

      Json.toJson(expected).mustEqual(expectedJson)

      nesting.Test.format.reads(expectedJson).mustEqual(JsSuccess(expected))
    }

    "handle case class with generic type and default field" in {
      implicit val format: Format[BigGenericCaseClassWithDefault[Int]] =
        Json.format[BigGenericCaseClassWithDefault[Int]]

      val expected =
        BigGenericCaseClassWithDefault(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 3)
      val expectedJson = pads(21) ++ Json.obj("data" -> 3, "descr" -> "something")

      Json
        .toJson(
          BigGenericCaseClassWithDefault(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 3)
        )
        .mustEqual(expectedJson)
      Json.fromJson(expectedJson).mustEqual(JsSuccess(expected))
    }

    "handle case class with generic type and overridden default field" in {
      implicit val format: Format[BigGenericCaseClassWithDefault[Int]] =
        Json.format[BigGenericCaseClassWithDefault[Int]]

      val expected = BigGenericCaseClassWithDefault(
        1,
        2,
        3,
        4,
        5,
        6,
        7,
        8,
        9,
        10,
        11,
        12,
        13,
        14,
        15,
        16,
        17,
        18,
        19,
        20,
        21,
        3,
        "foo"
      )
      val expectedJson = pads(21) ++ Json.obj("data" -> 3, "descr" -> "foo")

      Json
        .toJson(
          BigGenericCaseClassWithDefault(
            1,
            2,
            3,
            4,
            5,
            6,
            7,
            8,
            9,
            10,
            11,
            12,
            13,
            14,
            15,
            16,
            17,
            18,
            19,
            20,
            21,
            3,
            "foo"
          )
        )
        .mustEqual(expectedJson)
      Json.fromJson(expectedJson).mustEqual(JsSuccess(expected))
    }

    "field ordering" in {
      val instance: OWrites[BigFieldOrderTest] = Json.writes[BigFieldOrderTest]

      val value =
        BigFieldOrderTest(1, 2, 3, Some(4), 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23)

      instance
        .writes(value)
        .fields
        .mustEqual(
          Seq(
            "x1"  -> 1,
            "x2"  -> 2,
            "x3"  -> 3,
            "x4"  -> 4,
            "x5"  -> 5,
            "x6"  -> 6,
            "x7"  -> 7,
            "x8"  -> 8,
            "x9"  -> 9,
            "x10" -> 10,
            "x11" -> 11,
            "x12" -> 12,
            "x13" -> 13,
            "x14" -> 14,
            "x15" -> 15,
            "x16" -> 16,
            "x17" -> 17,
            "x18" -> 18,
            "x19" -> 19,
            "x20" -> 20,
            "x21" -> 21,
            "x22" -> 22,
            "x23" -> 23,
          ).map { case (k, v) => k -> JsNumber(v) }
        )
      assert(instance.writes(value).value.isInstanceOf[ImmutableLinkedHashMap[?, ?]])
    }
  }
}

object MacroScala2Spec {

  case class WithImplicit1(pos: Int, text: String)(implicit
      x: Numeric[Int]
  ) { def x1 = x.one }
  case class WithImplicit2[N: Numeric](ident: String, value: N)

  case class BigSimple(
      bar: String,
      pad1: Int,
      pad2: Int,
      pad3: Int,
      pad4: Int,
      pad5: Int,
      pad6: Int,
      pad7: Int,
      pad8: Int,
      pad9: Int,
      pad10: Int,
      pad11: Int,
      pad12: Int,
      pad13: Int,
      pad14: Int,
      pad15: Int,
      pad16: Int,
      pad17: Int,
      pad18: Int,
      pad19: Int,
      pad20: Int,
      pad21: Int,
      pad22: Int,
  )

  case class BigOptional(
      prop: Option[String],
      pad1: Option[Int],
      pad2: Option[Int],
      pad3: Option[Int],
      pad4: Option[Int],
      pad5: Option[Int],
      pad6: Option[Int],
      pad7: Option[Int],
      pad8: Option[Int],
      pad9: Option[Int],
      pad10: Option[Int],
      pad11: Option[Int],
      pad12: Option[Int],
      pad13: Option[Int],
      pad14: Option[Int],
      pad15: Option[Int],
      pad16: Option[Int],
      pad17: Option[Int],
      pad18: Option[Int],
      pad19: Option[Int],
      pad20: Option[Int],
      pad21: Option[Int],
      pad22: Option[Int],
  )

  object BigFoo {
    type Id = String
    def id(value: String): Id = value.asInstanceOf[Id]

    implicit val idReads: Reads[Id] = implicitly[Reads[String]].asInstanceOf[Reads[Id]]
  }
  case class BigFoo(
      id: BigFoo.Id,
      value: Option[Either[String, BigFoo]],
      pad1: Int,
      pad2: Int,
      pad3: Int,
      pad4: Int,
      pad5: Int,
      pad6: Int,
      pad7: Int,
      pad8: Int,
      pad9: Int,
      pad10: Int,
      pad11: Int,
      pad12: Int,
      pad13: Int,
      pad14: Int,
      pad15: Int,
      pad16: Int,
      pad17: Int,
      pad18: Int,
      pad19: Int,
      pad20: Int,
      pad21: Int,
  )

  case class BigInterval[T](
      base: T,
      other: Option[T],
      pad1: Int,
      pad2: Int,
      pad3: Int,
      pad4: Int,
      pad5: Int,
      pad6: Int,
      pad7: Int,
      pad8: Int,
      pad9: Int,
      pad10: Int,
      pad11: Int,
      pad12: Int,
      pad13: Int,
      pad14: Int,
      pad15: Int,
      pad16: Int,
      pad17: Int,
      pad18: Int,
      pad19: Int,
      pad20: Int,
      pad21: Int,
  )

  case class BigComplex[T, U](
      id: Int,
      a: T,
      b: Either[T, String],
      c: U,
      pad1: Int,
      pad2: Int,
      pad3: Int,
      pad4: Int,
      pad5: Int,
      pad6: Int,
      pad7: Int,
      pad8: Int,
      pad9: Int,
      pad10: Int,
      pad11: Int,
      pad12: Int,
      pad13: Int,
      pad14: Int,
      pad15: Int,
      pad16: Int,
      pad17: Int,
      pad18: Int,
      pad19: Int,
      pad20: Int,
  )

  case class BigGenericCaseClassWithDefault[A](
      pad1: Int,
      pad2: Int,
      pad3: Int,
      pad4: Int,
      pad5: Int,
      pad6: Int,
      pad7: Int,
      pad8: Int,
      pad9: Int,
      pad10: Int,
      pad11: Int,
      pad12: Int,
      pad13: Int,
      pad14: Int,
      pad15: Int,
      pad16: Int,
      pad17: Int,
      pad18: Int,
      pad19: Int,
      pad20: Int,
      pad21: Int,
      data: A,
      descr: String = "something",
  )

  case class BigFieldOrderTest(
      x1: Int,
      x2: Int,
      x3: Int,
      x4: Option[Int],
      x5: Int,
      x6: Int,
      x7: Int,
      x8: Int,
      x9: Int,
      x10: Int,
      x11: Int,
      x12: Int,
      x13: Int,
      x14: Int,
      x15: Int,
      x16: Int,
      x17: Int,
      x18: Int,
      x19: Int,
      x20: Int,
      x21: Int,
      x22: Int,
      x23: Int,
  )

  class BigNestingClass {
    case class Test(
        underlying: Option[TextId],
        pad1: Int,
        pad2: Int,
        pad3: Int,
        pad4: Int,
        pad5: Int,
        pad6: Int,
        pad7: Int,
        pad8: Int,
        pad9: Int,
        pad10: Int,
        pad11: Int,
        pad12: Int,
        pad13: Int,
        pad14: Int,
        pad15: Int,
        pad16: Int,
        pad17: Int,
        pad18: Int,
        pad19: Int,
        pad20: Int,
        pad21: Int,
        pad22: Int,
    )

    object Test {
      implicit def format(implicit textIdFormat: Format[TextId]): Format[Test] =
        Json.format[Test]
    }
  }
}
