/*
 * Copyright (C) 2009-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import org.scalacheck.Gen

class MacroScala2Spec extends AnyWordSpec with Matchers with org.scalatestplus.scalacheck.ScalaCheckPropertyChecks {
  import MacroScala2Spec._

  "Reads" should {
    "be generated for simple case class" in {
      val json     = Json.obj("bar" -> "lorem")
      val expected = Simple("lorem")

      forAll(
        Gen.oneOf(
          Json.reads[Simple],
          Json.configured.reads[Simple],
          Json.using[Json.MacroOptions].reads[Simple]
        )
      ) { _.reads(json).mustEqual(JsSuccess(expected)) }
    }

    "be generated for a sealed family" when {
      // lampepfl/dotty-feature-requests#161 No Mirror for sealed hierarchies that contain sealed trait children
      "subtype is a sealed trait itself" in {
        val expected   = Leaf2(1)
        val expectedJs = Json.obj("_type" -> "play.api.libs.json.MacroScala2Spec.Leaf2", "value" -> 1)

        Json.toJson[TreeValue](expected).mustEqual(expectedJs)
        expectedJs.validate[TreeValue].mustEqual(JsSuccess(expected))
      }
    }

    // lampepfl/dotty#7000 No Mirrors for value classes
    "be generated for a ValueClass" in {
      val expected                  = new TextId("foo")
      implicit val r: Reads[TextId] = Json.valueReads

      JsString("foo").validate[TextId].mustEqual(JsSuccess(expected))
    }
  }

  "Writes" should {
    // lampepfl/dotty#7000 No Mirrors for value classes
    "be generated for a ValueClass" in {
      val js = Json.valueWrites[TextId].writes(new TextId("bar"))

      js.mustEqual(JsString("bar"))
    }
  }

  "Macro" should {
    // lampepfl/dotty#11054 Type aliasing breaks constValue
    "handle case class with self type as nested type parameter" when {
      import TestFormats._

      val jsonNoValue  = Json.obj("id" -> "A")
      val jsonStrValue = Json.obj("id" -> "B", "value" -> "str")
      val jsonFooValue = Json.obj("id" -> "C", "value" -> jsonStrValue)

      val fooStrValue = Foo(Foo.id("B"), Some(Left("str")))
      val fooFooValue = Foo(Foo.id("C"), Some(Right(fooStrValue)))

      def readSpec(r: Reads[Foo]) = {
        r.reads(jsonNoValue).mustEqual(JsSuccess(Foo(Foo.id("A"), None)))
        r.reads(jsonStrValue).mustEqual(JsSuccess(fooStrValue))
        r.reads(jsonFooValue).mustEqual(JsSuccess(fooFooValue))
        r.reads(Json.obj("id" -> "D", "value" -> jsonFooValue))
          .mustEqual(JsSuccess(Foo(Foo.id("D"), Some(Right(fooFooValue)))))
      }

      def writeSpec(w: Writes[Foo]) = {
        w.writes(Foo(Foo.id("A"), None)).mustEqual(jsonNoValue)
        w.writes(fooStrValue).mustEqual(jsonStrValue)
        w.writes(fooFooValue).mustEqual(jsonFooValue)
        w.writes(Foo(Foo.id("D"), Some(Right(fooFooValue)))).mustEqual(Json.obj("id" -> "D", "value" -> jsonFooValue))
      }

      "to generate Reads" in readSpec(Json.reads[Foo])

      "to generate Writes" in writeSpec(Json.writes[Foo])

      "to generate Format" in {
        val f: OFormat[Foo] = Json.format[Foo]

        readSpec(f)
        writeSpec(f)
      }
    }

    // lampepfl/dotty-feature-requests#162 No support in Mirror for default arguments
    "handle case class with default values" when {
      val json01   = Json.obj("id" -> 15)
      val json02   = Json.obj("id" -> 15, "a" -> "a")
      val json03   = Json.obj("id" -> 15, "a" -> "a", "b" -> "b")
      val fixture0 = WithDefault(15, "a", Some("b"))

      val json1    = Json.obj("id" -> 15, "b" -> JsNull)
      val fixture1 = WithDefault(15, "a", None)

      val json2    = Json.obj("id" -> 15, "a" -> "aa")
      val fixture2 = WithDefault(15, "aa", Some("b"))

      val json3    = Json.obj("id" -> 15, "a" -> "aa", "b" -> "bb")
      val fixture3 = WithDefault(15, "aa", Some("bb"))

      val json4    = Json.obj("id" -> 18)
      val fixture4 = WithDefault(18)

      def readSpec(r: Reads[WithDefault]) = {
        r.reads(json01).mustEqual(JsSuccess(fixture0))
        r.reads(json02).mustEqual(JsSuccess(fixture0))
        r.reads(json03).mustEqual(JsSuccess(fixture0))
        r.reads(json1).mustEqual(JsSuccess(fixture1))
        r.reads(json2).mustEqual(JsSuccess(fixture2))
        r.reads(json3).mustEqual(JsSuccess(fixture3))
        r.reads(json4).mustEqual(JsSuccess(fixture4))
      }

      val jsWithDefaults = Json.using[Json.WithDefaultValues]

      "to generate Reads" in readSpec(
        jsWithDefaults.reads[WithDefault]
      )

      "to generate Format" in readSpec(
        jsWithDefaults.format[WithDefault]
      )
    }

    // lampepfl/dotty-feature-requests#162 No support in Mirror for default arguments
    "handle case class with default values, format defined in companion object" in {
      val json     = Json.obj("id" -> 15)
      val expected = WithDefaultInCompanion(15, "a")
      Json.fromJson[WithDefaultInCompanion](json).mustEqual(JsSuccess(expected))
    }

    // lampepfl/dotty-feature-requests#162 No support in Mirror for default arguments
    "handle case class with default values inner optional case class containing default values" when {
      implicit val withDefaultFormat: OFormat[WithDefault] =
        Json.using[Json.MacroOptions with Json.DefaultValues].format[WithDefault]

      val json01 = Json.obj("id" -> 3)
      val json02 = Json.obj(
        "id" -> 3,
        "ref" -> Json.obj(
          "id" -> 1
        )
      )
      val json03 = Json.obj(
        "id" -> 3,
        "ref" -> Json.obj(
          "id" -> 1,
          "a"  -> "a",
          "b"  -> "b"
        )
      )
      val fixture0 = ComplexWithDefault(3)

      val json11   = Json.obj("id" -> 15, "ref" -> JsNull)
      val fixture1 = ComplexWithDefault(15, None)

      def readSpec(r: Reads[ComplexWithDefault]) = {
        r.reads(json01).mustEqual(JsSuccess(fixture0))
        r.reads(json02).mustEqual(JsSuccess(fixture0))
        r.reads(json03).mustEqual(JsSuccess(fixture0))
        r.reads(json11).mustEqual(JsSuccess(fixture1))
      }

      val jsWithDefaults = Json.using[Json.WithDefaultValues]

      "to generate Reads" in readSpec(
        jsWithDefaults.reads[ComplexWithDefault]
      )

      "to generate Format" in readSpec(
        jsWithDefaults.format[ComplexWithDefault]
      )
    }

    // lampepfl/dotty-feature-requests#163 No Mirrors for case classes with implicits
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

    // lampepfl/dotty-feature-requests#163 No Mirrors for case classes with implicits
    "handle case class with collection types" when {
      import TestFormats._

      val json = Json.obj(
        "id"  -> "foo",
        "ls"  -> List(1.2D, 23.45D),
        "set" -> List(1, 3, 4, 7),
        "seq" -> List(
          Json.obj("_1" -> 2, "_2"       -> "bar"),
          Json.obj("_1" -> 4, "_2"       -> "lorem"),
          Json.obj("_2" -> "ipsum", "_1" -> 5)
        ),
        "scores" -> Json.obj("A1" -> 0.1F, "EF" -> 12.3F)
      )
      val withColl = WithColl(
        id = "foo",
        ls = List(1.2D, 23.45D),
        set = Set(1, 3, 4, 7),
        seq = Seq(2 -> "bar", 4 -> "lorem", 5 -> "ipsum"),
        scores = Map("A1" -> 0.1F, "EF" -> 12.3F)
      )

      def readSpec(r: Reads[WithColl[Double, (Int, String)]]) =
        r.reads(json).mustEqual(JsSuccess(withColl))

      def writeSpec(w: Writes[WithColl[Double, (Int, String)]]) =
        w.writes(withColl).mustEqual(json)

      "to generated Reads" in readSpec {
        Json.reads[WithColl[Double, (Int, String)]]
      }

      "to generated Writes".taggedAs(UnstableInScala213) in writeSpec {
        Json.writes[WithColl[Double, (Int, String)]]
      }

      "to generate Format".taggedAs(UnstableInScala213) in {
        val f = Json.format[WithColl[Double, (Int, String)]]
        readSpec(f)
        writeSpec(f)
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
}

object MacroScala2Spec {
  sealed trait Family
  case class Simple(bar: String)            extends Family
  case class Lorem[T](ipsum: T, age: Int)   extends Family
  case class Optional(prop: Option[String]) extends Family

  object FamilyCodec {
    implicit val simpleWrites: OWrites[Simple]     = Json.writes[Simple]
    implicit val optionalWrites: OWrites[Optional] = Json.writes[Optional]

    implicit val familyWrites: OWrites[Family] = Json.writes[Family] // Failing:
    /* java.lang.IllegalArgumentException:
     requirement failed: familyWrites  is not a valid identifier
     */
  }

  sealed trait TreeValue

  sealed trait SubLevel extends TreeValue

  case class Leaf1(value: String) extends TreeValue
  case class Leaf2(value: Int)    extends SubLevel

  object TreeValue {
    private implicit val leaf1: OFormat[Leaf1]       = Json.format
    private implicit val leaf2: OFormat[Leaf2]       = Json.format
    private implicit val subLevel: OFormat[SubLevel] = Json.format

    implicit val format: OFormat[TreeValue] = Json.format
  }

  object Foo {
    import shapeless.tag.@@

    type Id = String @@ Foo
    def id(value: String): Id = value.asInstanceOf[Id]

    implicit val idReads: Reads[Id] = implicitly[Reads[String]].asInstanceOf[Reads[Id]]
  }
  case class Foo(id: Foo.Id, value: Option[Either[String, Foo]])

  case class WithDefault(id: Int, a: String = "a", b: Option[String] = Some("b"))
  case class ComplexWithDefault(id: Int, ref: Option[WithDefault] = Some(WithDefault(1)))

  case class WithImplicit1(pos: Int, text: String)(implicit
      x: Numeric[Int]
  ) { def x1 = x.one }
  case class WithImplicit2[N: Numeric](ident: String, value: N)

  case class WithColl[A: Numeric, B](
      id: String,
      ls: List[A],
      set: Set[Int],
      seq: Seq[B],
      scores: Map[String, Float]
  )

  case class WithDefaultInCompanion(id: Int, a: String = "a")

  object WithDefaultInCompanion {

    implicit val format: OFormat[WithDefaultInCompanion] =
      Json.using[Json.WithDefaultValues].format[WithDefaultInCompanion]
  }
}
