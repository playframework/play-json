/*
 * Copyright (C) 2009-2020 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
object TestFormats {
  implicit def eitherReads[A: Reads, B: Reads] =
    Reads[Either[A, B]] { js =>
      implicitly[Reads[A]].reads(js) match {
        case JsSuccess(a, _) => JsSuccess(Left(a))
        case _               => implicitly[Reads[B]].reads(js).map(Right(_))
      }
    }

  implicit def eitherWrites[A: Writes, B: Writes] =
    Writes[Either[A, B]] {
      case Left(a)  => implicitly[Writes[A]].writes(a)
      case Right(b) => implicitly[Writes[B]].writes(b)
    }

  implicit def tuple2Reads[A: Reads, B: Reads]: Reads[(A, B)] =
    Reads { js =>
      for {
        a <- (js \ "_1").validate[A]
        b <- (js \ "_2").validate[B]
      } yield a -> b
    }

  implicit def tuple2OWrites[A: Writes, B: Writes]: OWrites[(A, B)] =
    OWrites {
      case (a, b) => Json.obj("_1" -> a, "_2" -> b)
    }
}

final class TextId(val value: String) extends AnyVal

import org.scalatest._
import org.scalacheck.Gen

class MacroSpec extends AnyWordSpec with Matchers with org.scalatestplus.scalacheck.ScalaCheckPropertyChecks {
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
      ) { _.reads(json).get.mustEqual(expected) }
    }

    "as Format for a simple generic case class" in {
      val fmt: OFormat[Lorem[Double]] = Json.format

      fmt
        .reads(Json.obj("ipsum" -> 0.123d, "age" -> 1))
        .get
        .mustEqual(
          Lorem(
            0.123d,
            1
          )
        )
    }

    "refuse value other than JsObject when properties are optional" in {
      forAll(Gen.oneOf(Json.reads[Optional], Json.format[Optional])) { r =>
        r.reads(Json.obj()).get.mustEqual(Optional(None))

        (r.reads(JsString("foo")).asEither match {
          case Left((_, Seq(err)) :: Nil) =>
            err.message == "error.expected.jsobject"

          case _ => false
        }).mustEqual(true)
      }
    }

    "ignore Option alias" in {
      def a = {
        implicit lazy val _: Reads[OptionalInt] = ???

        Json.reads[UsingAlias]
      }

      def b = {
        implicit lazy val _: Format[OptionalInt] = ???

        Json.reads[UsingAlias]
      }

      def c = {
        implicit lazy val _: Reads[OptionalInt] = ???

        Json.format[UsingAlias]
      }

      def d = {
        implicit lazy val _: Format[OptionalInt] = ???

        Json.format[UsingAlias]
      }

      forAll(Gen.oneOf(a, b, c, d)) { r =>
        r.reads(Json.obj("v" -> 1)).get.mustEqual(UsingAlias(Some(1)))
        r.reads(Json.obj()).get.mustEqual(UsingAlias(None))
      }
    }

    "be generated for a sealed family" when {
      implicit val simpleReads = Reads[Simple] { js =>
        (js \ "bar").validate[String].map(Simple(_))
      }
      implicit val optionalReads: Reads[Optional] = Json.reads
      implicit val familyReads: Reads[Family]     = Json.reads

      val simple   = Simple("foo")
      val optional = Optional(None)

      shapeless.test.illTyped("Json.reads[EmptyFamily]")
      shapeless.test.illTyped("Json.writes[EmptyFamily]")
      shapeless.test.illTyped("Json.format[EmptyFamily]")

      "using the _value syntax" in {
        val jsSimple = Json.obj(
          "_type"  -> "play.api.libs.json.MacroSpec.Simple",
          "_value" -> Json.writes[Simple].writes(simple)
        )

        val jsOptional = Json.obj(
          "_type"  -> "play.api.libs.json.MacroSpec.Optional",
          "_value" -> Json.writes[Optional].writes(optional)
        )

        jsSimple.validate[Family].get.mustEqual(simple)
        jsOptional.validate[Family].get.mustEqual(optional)
      }

      "using the compact syntax" in {
        val jsSimple = Json.writes[Simple].writes(simple) + (
          "_type" -> JsString("play.api.libs.json.MacroSpec.Simple")
        )

        val jsOptional = Json.writes[Optional].writes(optional) + (
          "_type" -> JsString("play.api.libs.json.MacroSpec.Optional")
        )

        jsSimple.validate[Family].get.mustEqual(simple)
        jsOptional.validate[Family].get.mustEqual(optional)
      }
    }

    "be generated for a ValueClass" in {
      val expected                  = new TextId("foo")
      implicit val r: Reads[TextId] = Json.valueReads

      JsString("foo").validate[TextId].get.mustEqual(expected)
    }
  }

  "Writes" should {
    "be generated for simple case class" in {
      Json.writes[Simple].writes(Simple("lorem")).mustEqual(Json.obj("bar" -> "lorem"))
    }

    "as Format for a generic case class" in {
      val fmt: Format[Lorem[Float]] = Json.format

      fmt
        .writes(Lorem(2.34f, 2))
        .mustEqual(
          Json.obj(
            "ipsum" -> 2.34f,
            "age"   -> 2
          )
        )
    }

    "ignore Option alias" in {
      def a = {
        implicit lazy val _: Writes[OptionalInt] = ???

        Json.writes[UsingAlias]
      }

      def b = {
        implicit lazy val _: Format[OptionalInt] = ???

        Json.writes[UsingAlias]
      }

      def c = {
        implicit lazy val _: Writes[OptionalInt] = ???

        Json.format[UsingAlias]
      }

      def d = {
        implicit lazy val _: Format[OptionalInt] = ???

        Json.format[UsingAlias]
      }

      forAll(Gen.oneOf(a, b, c, d)) { r =>
        r.writes(UsingAlias(Some(1))).mustEqual(Json.obj("v" -> 1))
        r.writes(UsingAlias(None)).mustEqual(Json.obj())
      }
    }

    "be generated for a sealed family" in {
      implicit val simpleWrites = Writes[Simple] { simple =>
        Json.obj("bar" -> simple.bar)
      }

      implicit val optionalWrites = Json.writes[Optional]
      // Following won't work due to inferrence issue (see #117)
      // with inheritance/contravariance/implicit resolution
      //val _: OWrites[Optional] = Json.writes

      implicit val familyWrites: OWrites[Family] = Json.writes[Family]

      val simple   = Simple("foo")
      val optional = Optional(None)

      val jsSimple = simpleWrites.writes(simple).asInstanceOf[JsObject] + (
        "_type" -> JsString("play.api.libs.json.MacroSpec.Simple")
      )

      val jsOptional = optionalWrites.writes(optional) + (
        "_type" -> JsString("play.api.libs.json.MacroSpec.Optional")
      )

      lazy val wsimple = Json.toJson[Family](simple)
      lazy val wopt    = Json.toJson[Family](optional)

      wsimple.mustEqual(jsSimple)
      wsimple.validate(Json.reads[Simple]).get.mustEqual(simple)
      wopt.mustEqual(jsOptional)
      wopt.validate(Json.reads[Optional]).get.mustEqual(optional)

      // was StackOverFlow exception
      Json
        .toJson[Family1](Family1Member("bar"))
        .mustEqual(
          Json.obj(
            "_type" -> "play.api.libs.json.MacroSpec.Family1Member",
            "foo"   -> "bar"
          )
        )
    }

    "be generated for a ValueClass" in {
      val js = Json.valueWrites[TextId].writes(new TextId("bar"))

      js.mustEqual(JsString("bar"))
    }
  }

  "Macro" should {
    "handle case class with self type as nested type parameter" when {
      import TestFormats._

      val jsonNoValue  = Json.obj("id" -> "A")
      val jsonStrValue = Json.obj("id" -> "B", "value" -> "str")
      val jsonFooValue = Json.obj("id" -> "C", "value" -> jsonStrValue)

      val fooStrValue = Foo(Foo.id("B"), Some(Left("str")))
      val fooFooValue = Foo(Foo.id("C"), Some(Right(fooStrValue)))

      def readSpec(r: Reads[Foo]) = {
        r.reads(jsonNoValue).get.mustEqual(Foo(Foo.id("A"), None))
        r.reads(jsonStrValue).get.mustEqual(fooStrValue)
        r.reads(jsonFooValue).get.mustEqual(fooFooValue)
        r.reads(Json.obj("id" -> "D", "value" -> jsonFooValue))
          .get
          .mustEqual(Foo(Foo.id("D"), Some(Right(fooFooValue))))
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

    "handle generic case class with multiple generic parameters" when {
      val jsonNoOther = Json.obj("base" -> 1)
      val jsonOther   = Json.obj("base" -> 2, "other" -> 3)

      val noOther = Interval(1, None)
      val other   = Interval(2, Some(3))

      def readSpec(r: Reads[Interval[Int]]) = {
        r.reads(jsonNoOther).get.mustEqual(noOther)
        r.reads(jsonOther).get.mustEqual(other)
      }

      def writeSpec(r: Writes[Interval[Int]]) = {
        r.writes(noOther).mustEqual(jsonNoOther)
        r.writes(other).mustEqual(jsonOther)
      }

      "to generate Reads" in readSpec(Json.reads[Interval[Int]])

      "to generate Writes" in writeSpec(Json.writes[Interval[Int]])

      "to generate Format" in {
        val f = Json.format[Interval[Int]]
        readSpec(f)
        writeSpec(f)
      }
    }

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
        r.reads(json01).get.mustEqual(fixture0)
        r.reads(json02).get.mustEqual(fixture0)
        r.reads(json03).get.mustEqual(fixture0)
        r.reads(json1).get.mustEqual(fixture1)
        r.reads(json2).get.mustEqual(fixture2)
        r.reads(json3).get.mustEqual(fixture3)
        r.reads(json4).get.mustEqual(fixture4)
      }

      val jsWithDefaults = Json.using[Json.WithDefaultValues]

      "to generate Reads" in readSpec(
        jsWithDefaults.reads[WithDefault]
      )

      "to generate Format" in readSpec(
        jsWithDefaults.format[WithDefault]
      )
    }

    "handle case class with default values, format defined in companion object" in {
      val json     = Json.obj("id" -> 15)
      val expected = WithDefaultInCompanion(15, "a")
      Json.fromJson[WithDefaultInCompanion](json).get.mustEqual(expected)
    }

    "handle case class with default values inner optional case class containing default values" when {
      implicit val withDefaultFormat = Json.using[Json.MacroOptions with Json.DefaultValues].format[WithDefault]

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
        r.reads(json01).get.mustEqual(fixture0)
        r.reads(json02).get.mustEqual(fixture0)
        r.reads(json03).get.mustEqual(fixture0)
        r.reads(json11).get.mustEqual(fixture1)
      }

      val jsWithDefaults = Json.using[Json.WithDefaultValues]

      "to generate Reads" in readSpec(
        jsWithDefaults.reads[ComplexWithDefault]
      )

      "to generate Format" in readSpec(
        jsWithDefaults.format[ComplexWithDefault]
      )
    }

    "handle case class with implicits" when {
      val json1    = Json.obj("pos" -> 2, "text" -> "str")
      val json2    = Json.obj("ident" -> "id", "value" -> 23.456d)
      val fixture1 = WithImplicit1(2, "str")
      val fixture2 = WithImplicit2("id", 23.456d)

      def readSpec1(r: Reads[WithImplicit1]) =
        r.reads(json1).get.mustEqual(fixture1)

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
        f2.reads(json2).get.mustEqual(fixture2)
      }
    }

    "handle generic case class with multiple generic parameters and self references" when {
      import TestFormats._

      val nestedLeft = Json.obj("id" -> 2, "a" -> 0.2f, "b" -> 0.3f, "c" -> 3)

      val nestedRight = Json.obj(
        "id" -> 1,
        "a"  -> 0.1f,
        "b"  -> "right1",
        "c"  -> 2
      )

      val jsonRight = Json.obj(
        "id" -> 3,
        "a"  -> nestedRight,
        "b"  -> "right2",
        "c"  -> 0.4d
      )

      val jsonLeft = Json.obj(
        "id" -> 4,
        "a"  -> nestedLeft,
        "b"  -> nestedRight,
        "c"  -> 0.5d
      )

      val complexRight = Complex(3, Complex(1, 0.1f, Right("right1"), 2), Right("right2"), 0.4d)

      val complexLeft = Complex(4, Complex(2, 0.2f, Left(0.3f), 3), Left(Complex(1, 0.1f, Right("right1"), 2)), 0.5d)

      def readSpec(r: Reads[Complex[Complex[Float, Int], Double]]) = {
        r.reads(jsonRight).get.mustEqual(complexRight)
        r.reads(jsonLeft).get.mustEqual(complexLeft)
      }

      def writeSpec(r: Writes[Complex[Complex[Float, Int], Double]]) = {
        r.writes(complexRight).mustEqual(jsonRight)
        r.writes(complexLeft).mustEqual(jsonLeft)
      }

      "to generate Reads" in readSpec {
        implicit val nested = Json.reads[Complex[Float, Int]]
        Json.reads[Complex[Complex[Float, Int], Double]]
      }

      "to generate Writes" in writeSpec {
        implicit val nested = Json.writes[Complex[Float, Int]]
        Json.writes[Complex[Complex[Float, Int], Double]]
      }

      "to generate Format" in {
        implicit val nested = Json.format[Complex[Float, Int]]
        val f               = Json.format[Complex[Complex[Float, Int], Double]]

        readSpec(f)
        writeSpec(f)
      }
    }

    "handle case class with collection types" when {
      import TestFormats._

      val json = Json.obj(
        "id"  -> "foo",
        "ls"  -> List(1.2d, 23.45d),
        "set" -> List(1, 3, 4, 7),
        "seq" -> List(
          Json.obj("_1" -> 2, "_2"       -> "bar"),
          Json.obj("_1" -> 4, "_2"       -> "lorem"),
          Json.obj("_2" -> "ipsum", "_1" -> 5)
        ),
        "scores" -> Json.obj("A1" -> 0.1f, "EF" -> 12.3f)
      )
      val withColl = WithColl(
        id = "foo",
        ls = List(1.2d, 23.45d),
        set = Set(1, 3, 4, 7),
        seq = Seq(2 -> "bar", 4 -> "lorem", 5 -> "ipsum"),
        scores = Map("A1" -> 0.1f, "EF" -> 12.3f)
      )

      def readSpec(r: Reads[WithColl[Double, (Int, String)]]) =
        r.reads(json).get.mustEqual(withColl)

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

    "handle sealed family" in {
      implicit val simpleWrites = OWrites[Simple] { simple =>
        Json.obj("bar" -> simple.bar)
      }
      implicit val simpleReads = Reads[Simple] { js =>
        (js \ "bar").validate[String].map(Simple(_))
      }
      implicit val optionalFormat: OFormat[Optional] = Json.format[Optional]
      implicit val familyFormat: OFormat[Family]     = Json.format[Family]

      val simple = Simple("foo")
      val jsSimple = simpleWrites.writes(simple) + (
        "_type" -> JsString("play.api.libs.json.MacroSpec.Simple")
      )

      val optional = Optional(None)
      val jsOptional = optionalFormat.writes(optional) + (
        "_type" -> JsString("play.api.libs.json.MacroSpec.Optional")
      )

      Json.toJson[Family](simple).mustEqual(jsSimple)
      Json.toJson[Family](optional).mustEqual(jsOptional)
      jsSimple.validate[Family].get.mustEqual(simple)
      jsOptional.validate[Family].get.mustEqual(optional)
      jsSimple.validate(Json.reads[Simple]).get.mustEqual(simple)
      jsOptional.validate(Json.reads[Optional]).get.mustEqual(optional)
    }

    "handle sealed family with custom discriminator name" in {
      implicit val cfg = JsonConfiguration(discriminator = "_discriminator")
      implicit val simpleWrites = OWrites[Simple] { simple =>
        Json.obj("bar" -> simple.bar)
      }
      implicit val simpleReads = Reads[Simple] { js =>
        (js \ "bar").validate[String].map(Simple(_))
      }
      implicit val optionalFormat: OFormat[Optional] = Json.format[Optional]
      implicit val familyFormat: OFormat[Family]     = Json.format[Family]

      val simple = Simple("foo")
      val jsSimple = simpleWrites.writes(simple) + (
        "_discriminator" -> JsString("play.api.libs.json.MacroSpec.Simple")
      )

      val optional = Optional(None)
      val jsOptional = optionalFormat.writes(optional) + (
        "_discriminator" -> JsString("play.api.libs.json.MacroSpec.Optional")
      )

      Json.toJson[Family](simple).mustEqual(jsSimple)
      Json.toJson[Family](optional).mustEqual(jsOptional)
      jsSimple.validate[Family].get.mustEqual(simple)
      jsOptional.validate[Family].get.mustEqual(optional)
      jsSimple.validate(Json.reads[Simple]).get.mustEqual(simple)
      jsOptional.validate(Json.reads[Optional]).get.mustEqual(optional)
    }

    "handle sealed family with typeNaming" in {
      implicit val cfg = JsonConfiguration(typeNaming = JsonNaming {
        case "play.api.libs.json.MacroSpec.Simple"   => "simple"
        case "play.api.libs.json.MacroSpec.Optional" => "optional"
      })
      implicit val simpleWrites = OWrites[Simple] { simple =>
        Json.obj("bar" -> simple.bar)
      }
      implicit val simpleReads = Reads[Simple] { js =>
        (js \ "bar").validate[String].map(Simple(_))
      }
      implicit val optionalFormat: OFormat[Optional] = Json.format[Optional]
      implicit val familyFormat: OFormat[Family]     = Json.format[Family]

      val simple = Simple("foo")
      val jsSimple = simpleWrites.writes(simple) + (
        "_type" -> JsString("simple")
      )

      val optional = Optional(None)
      val jsOptional = optionalFormat.writes(optional) + (
        "_type" -> JsString("optional")
      )

      Json.toJson[Family](simple).mustEqual(jsSimple)
      Json.toJson[Family](optional).mustEqual(jsOptional)
      jsSimple.validate[Family].get.mustEqual(simple)
      jsOptional.validate[Family].get.mustEqual(optional)
      jsSimple.validate(Json.reads[Simple]).get.mustEqual(simple)
      jsOptional.validate(Json.reads[Optional]).get.mustEqual(optional)
    }

    "handle case objects as empty JsObject" in {
      case object Obj
      val writer    = Json.writes[Obj.type]
      val reader    = Json.reads[Obj.type]
      val formatter = Json.format[Obj.type]

      val jsObj = Json.obj()
      writer.writes(Obj).mustEqual(jsObj)
      reader.reads(jsObj).mustEqual(JsSuccess(Obj))
      formatter.writes(Obj).mustEqual(jsObj)
      formatter.reads(jsObj).mustEqual(JsSuccess(Obj))
    }

    "handle ValueClass" in {
      val id                           = new TextId("foo")
      val js                           = JsString("foo")
      implicit val fmt: Format[TextId] = Json.valueFormat[TextId]

      js.validate[TextId].get.mustEqual(id)
      fmt.writes(id).mustEqual(js)
    }
  }

  // ---

  sealed trait Family
  case class Simple(bar: String)            extends Family
  case class Lorem[T](ipsum: T, age: Int)   extends Family
  case class Optional(prop: Option[String]) extends Family

  sealed trait EmptyFamily

  object FamilyCodec {
    implicit val simpleWrites   = Json.writes[Simple]
    implicit val optionalWrites = Json.writes[Optional]

    implicit val familyWrites = Json.writes[Family] // Failing:
    /* java.lang.IllegalArgumentException:
     requirement failed: familyWrites  is not a valid identifier
     */
  }

  object Foo {
    import shapeless.tag.@@

    type Id = String @@ Foo
    def id(value: String): Id = value.asInstanceOf[Id]

    implicit val idReads: Reads[Id] = implicitly[Reads[String]].asInstanceOf[Reads[Id]]
  }
  case class Foo(id: Foo.Id, value: Option[Either[String, Foo]])

  case class Interval[T](base: T, other: Option[T])
  case class Complex[T, U](id: Int, a: T, b: Either[T, String], c: U)

  case class WithDefault(id: Int, a: String = "a", b: Option[String] = Some("b"))
  case class ComplexWithDefault(id: Int, ref: Option[WithDefault] = Some(WithDefault(1)))

  case class WithImplicit1(pos: Int, text: String)(implicit x: Numeric[Int])
  case class WithImplicit2[N: Numeric](ident: String, value: N)

  case class WithColl[A: Numeric, B](
      id: String,
      ls: List[A],
      set: Set[Int],
      seq: Seq[B],
      scores: Map[String, Float]
  )

  type OptionalInt = Option[Int]
  case class UsingAlias(v: OptionalInt)

  case class WithDefaultInCompanion(id: Int, a: String = "a")
  object WithDefaultInCompanion {
    implicit val format: OFormat[WithDefaultInCompanion] =
      Json.using[Json.WithDefaultValues].format[WithDefaultInCompanion]
  }

  // ---

  sealed trait Family1
  object Family1 {
    def w: OWrites[Family1]                    = Json.writes[Family1]
    implicit lazy val writes: OWrites[Family1] = w
  }

  case class Family1Member(foo: String) extends Family1
  object Family1Member {
    implicit def writer: OWrites[Family1Member] = Json.writes[Family1Member]
  }

  // ---

  sealed trait Family2
  case class Family2Member(p: Int) extends Family2
  object Family2 {
    implicit def w: OWrites[Family2] = {
      shapeless.test.illTyped("Json.writes[Family2]")
      ???
    }
    /* Should fail, as there is no implicit for Family2Member,
     for now due to the contravariance `w` being defined is self resolved
     as Writes instance this subtype Family2Member
     */
  }
}
