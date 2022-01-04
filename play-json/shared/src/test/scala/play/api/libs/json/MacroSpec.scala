/*
 * Copyright (C) 2009-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import org.scalatest.matchers.must.Matchers
import Matchers._
import org.scalatest.wordspec.AnyWordSpec

object TestFormats {
  implicit def eitherReads[A: Reads, B: Reads]: Reads[Either[A, B]] = Reads[Either[A, B]] { js =>
    implicitly[Reads[A]].reads(js).map(Left(_)).orElse(implicitly[Reads[B]].reads(js).map(Right(_)))
  }

  implicit def eitherWrites[A: Writes, B: Writes]: Writes[Either[A, B]] = Writes[Either[A, B]] {
    case Left(a)  => implicitly[Writes[A]].writes(a)
    case Right(b) => implicitly[Writes[B]].writes(b)
  }

  implicit def tuple2Reads[A: Reads, B: Reads]: Reads[(A, B)] = Reads { js =>
    for {
      a <- (js \ "_1").validate[A]
      b <- (js \ "_2").validate[B]
    } yield a -> b
  }

  implicit def tuple2OWrites[A: Writes, B: Writes]: OWrites[(A, B)] = OWrites {
    case (a, b) =>
      Json.obj("_1" -> a, "_2" -> b)
  }
}

final class TextId(val value: String) extends AnyVal

import org.scalacheck.Gen

class MacroSpec extends AnyWordSpec with Matchers with org.scalatestplus.scalacheck.ScalaCheckPropertyChecks {
  import MacroSpec._

  "Reads" should {
    "be generated for simple case class" in {
      val json     = Json.obj("bar" -> "lorem")
      val expected = Simple("lorem")

      forAll(
        Json.reads[Simple],
      ) { _.reads(json).mustEqual(JsSuccess(expected)) }
    }

    "as Format for a simple generic case class" in {
      val fmt: OFormat[Lorem[Double]] = Json.format

      fmt.reads(Json.obj("ipsum" -> 0.123D, "age" -> 1)).mustEqual(JsSuccess(Lorem(0.123D, 1)))
    }

    "refuse value other than JsObject when properties are optional" in {
      forAll(Gen.oneOf(Json.reads[Optional], Json.format[Optional])) { r =>
        r.reads(Json.obj()).mustEqual(JsSuccess(Optional(None)))

        (r.reads(JsString("foo")).asEither match {
          case Left((_, err :: Nil) :: Nil) =>
            err.message == "error.expected.jsobject"

          case _ => false
        }).mustEqual(true)
      }
    }

    "ignore Option alias" in {
      def a: Reads[UsingAlias] = {
        implicit lazy val x: Reads[OptionalInt] = ???

        Json.reads[UsingAlias]
      }

      def b: Reads[UsingAlias] = {
        implicit lazy val x: Format[OptionalInt] = ???

        Json.reads[UsingAlias]
      }

      def c: OFormat[UsingAlias] = {
        implicit lazy val x: Reads[OptionalInt] = ???

        Json.format[UsingAlias]
      }

      def d: OFormat[UsingAlias] = {
        implicit lazy val x: Format[OptionalInt] = ???

        Json.format[UsingAlias]
      }

      forAll(Gen.oneOf(a, b, c, d)) { r =>
        r.reads(Json.obj("v" -> 1)).mustEqual(JsSuccess(UsingAlias(Some(1))))
        r.reads(Json.obj()).mustEqual(JsSuccess(UsingAlias(None)))
      }
    }

    "be generated for a sealed family" when {
      implicit val simpleReads: Reads[Simple] = Reads[Simple] { js =>
        (js \ "bar").validate[String].map(Simple(_))
      }
      // import LoremCodec.loremReads // Doesn't work in Scala 3.0.0-RC1
      implicit val loremReads: Reads[Lorem[Any]]  = LoremCodec.loremReads
      implicit val optionalReads: Reads[Optional] = Json.reads[Optional]
      implicit val familyReads: Reads[Family]     = Json.reads[Family]

      val simple   = Simple("foo")
      val optional = Optional(None)

      "Json.reads[EmptyFamily]".mustNot(typeCheck)
      "Json.writes[EmptyFamily]".mustNot(typeCheck)
      "Json.format[EmptyFamily]".mustNot(typeCheck)

      "using the _value syntax" in {
        val jsSimple = Json.obj(
          "_type"  -> "play.api.libs.json.MacroSpec.Simple",
          "_value" -> Json.writes[Simple].writes(simple)
        )

        val jsOptional = Json.obj(
          "_type"  -> "play.api.libs.json.MacroSpec.Optional",
          "_value" -> Json.writes[Optional].writes(optional)
        )

        jsSimple.validate[Family].mustEqual(JsSuccess(simple))
        jsOptional.validate[Family].mustEqual(JsSuccess(optional))
      }

      "using the compact syntax" in {
        val jsSimple = Json.writes[Simple].writes(simple) + (
          "_type" -> JsString("play.api.libs.json.MacroSpec.Simple")
        )

        val jsOptional = Json.writes[Optional].writes(optional) + (
          "_type" -> JsString("play.api.libs.json.MacroSpec.Optional")
        )

        jsSimple.validate[Family].mustEqual(JsSuccess(simple))
        jsOptional.validate[Family].mustEqual(JsSuccess(optional))
      }
    }
  }

  "Writes" should {
    "be generated for simple case class" in {
      Json.writes[Simple].writes(Simple("lorem")).mustEqual(Json.obj("bar" -> "lorem"))
    }

    "as Format for a generic case class" in {
      val fmt: Format[Lorem[Float]] = Json.format

      fmt
        .writes(Lorem(2.34F, 2))
        .mustEqual(
          Json.obj(
            "ipsum" -> 2.34F,
            "age"   -> 2
          )
        )
    }

    "ignore Option alias" in {
      def a: OWrites[UsingAlias] = {
        implicit lazy val x: Writes[OptionalInt] = ???

        Json.writes[UsingAlias]
      }

      def b: OWrites[UsingAlias] = {
        implicit lazy val x: Format[OptionalInt] = ???

        Json.writes[UsingAlias]
      }

      def c: OFormat[UsingAlias] = {
        implicit lazy val x: Writes[OptionalInt] = ???

        Json.format[UsingAlias]
      }

      def d: OFormat[UsingAlias] = {
        implicit lazy val x: Format[OptionalInt] = ???

        Json.format[UsingAlias]
      }

      forAll(Gen.oneOf(a, b, c, d)) { r =>
        r.writes(UsingAlias(Some(1))).mustEqual(Json.obj("v" -> 1))
        r.writes(UsingAlias(None)).mustEqual(Json.obj())
      }
    }

    "be generated for a sealed family" in {
      implicit val simpleWrites: OWrites[Simple] = OWrites[Simple] { simple =>
        Json.obj("bar" -> simple.bar)
      }
      // import LoremCodec.loremWrites // Doesn't work in Scala 3.0.0-RC1
      implicit val loremWrites: OWrites[Lorem[Any]]  = LoremCodec.loremWrites
      implicit val optionalWrites: OWrites[Optional] = Json.writes[Optional]
      // Following won't work due to inference issue (see #117)
      // with inheritance/contravariance/implicit resolution
      // val _: OWrites[Optional] = Json.writes

      implicit val familyWrites: OWrites[Family] = Json.writes[Family]

      val simple   = Simple("foo")
      val optional = Optional(None)

      val jsSimple = simpleWrites.writes(simple) + (
        "_type" -> JsString("play.api.libs.json.MacroSpec.Simple")
      )

      val jsOptional = optionalWrites.writes(optional) + (
        "_type" -> JsString("play.api.libs.json.MacroSpec.Optional")
      )

      lazy val wsimple = Json.toJson[Family](simple)
      lazy val wopt    = Json.toJson[Family](optional)

      wsimple.mustEqual(jsSimple)
      wsimple.validate(Json.reads[Simple]).mustEqual(JsSuccess(simple))
      wopt.mustEqual(jsOptional)
      wopt.validate(Json.reads[Optional]).mustEqual(JsSuccess(optional))

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
  }

  "Macro" should {
    "handle case class with self type as nested type parameter" when {
      // import TestFormats._ // Doesn't work in Scala 3.0.0-RC1
      implicit def eitherReads[A: Reads, B: Reads]: Reads[Either[A, B]]     = TestFormats.eitherReads[A, B]
      implicit def eitherWrites[A: Writes, B: Writes]: Writes[Either[A, B]] = TestFormats.eitherWrites[A, B]
      implicit def tuple2Reads[A: Reads, B: Reads]: Reads[(A, B)]           = TestFormats.tuple2Reads[A, B]
      implicit def tuple2OWrites[A: Writes, B: Writes]: OWrites[(A, B)]     = TestFormats.tuple2OWrites[A, B]

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

    "handle generic case class with multiple generic parameters" when {
      val jsonNoOther = Json.obj("base" -> 1)
      val jsonOther   = Json.obj("base" -> 2, "other" -> 3)

      val noOther = Interval(1, None)
      val other   = Interval(2, Some(3))

      def readSpec(r: Reads[Interval[Int]]) = {
        r.reads(jsonNoOther).mustEqual(JsSuccess(noOther))
        r.reads(jsonOther).mustEqual(JsSuccess(other))
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

    "handle generic case class with multiple generic parameters and self references" when {
      // import TestFormats._ // Doesn't work in Scala 3.0.0-RC1
      implicit def eitherReads[A: Reads, B: Reads]: Reads[Either[A, B]]     = TestFormats.eitherReads[A, B]
      implicit def eitherWrites[A: Writes, B: Writes]: Writes[Either[A, B]] = TestFormats.eitherWrites[A, B]
      implicit def tuple2Reads[A: Reads, B: Reads]: Reads[(A, B)]           = TestFormats.tuple2Reads[A, B]
      implicit def tuple2OWrites[A: Writes, B: Writes]: OWrites[(A, B)]     = TestFormats.tuple2OWrites[A, B]

      val nestedLeft = Json.obj("id" -> 2, "a" -> 0.2F, "b" -> 0.3F, "c" -> 3)

      val nestedRight = Json.obj(
        "id" -> 1,
        "a"  -> 0.1F,
        "b"  -> "right1",
        "c"  -> 2
      )

      val jsonRight = Json.obj(
        "id" -> 3,
        "a"  -> nestedRight,
        "b"  -> "right2",
        "c"  -> 0.4D
      )

      val jsonLeft = Json.obj(
        "id" -> 4,
        "a"  -> nestedLeft,
        "b"  -> nestedRight,
        "c"  -> 0.5D
      )

      val complexRight = Complex(3, Complex(1, 0.1F, Right("right1"), 2), Right("right2"), 0.4D)

      val complexLeft = Complex(4, Complex(2, 0.2F, Left(0.3F), 3), Left(Complex(1, 0.1F, Right("right1"), 2)), 0.5D)

      def readSpec(r: Reads[Complex[Complex[Float, Int], Double]]) = {
        r.reads(jsonRight).mustEqual(JsSuccess(complexRight))
        r.reads(jsonLeft).mustEqual(JsSuccess(complexLeft))
      }

      def writeSpec(r: Writes[Complex[Complex[Float, Int], Double]]) = {
        r.writes(complexRight).mustEqual(jsonRight)
        r.writes(complexLeft).mustEqual(jsonLeft)
      }

      "to generate Reads" in readSpec {
        implicit val nested: Reads[Complex[Float, Int]] = Json.reads[Complex[Float, Int]]
        Json.reads[Complex[Complex[Float, Int], Double]]
      }

      "to generate Writes" in writeSpec {
        implicit val nested: OWrites[Complex[Float, Int]] = Json.writes[Complex[Float, Int]]
        Json.writes[Complex[Complex[Float, Int], Double]]
      }

      "to generate Format" in {
        implicit val nested: OFormat[Complex[Float, Int]] = Json.format[Complex[Float, Int]]
        val f                                             = Json.format[Complex[Complex[Float, Int], Double]]

        readSpec(f)
        writeSpec(f)
      }
    }

    "handle sealed family" in {
      implicit val simpleWrites: OWrites[Simple] = OWrites[Simple] { simple =>
        Json.obj("bar" -> simple.bar)
      }
      implicit val simpleReads: Reads[Simple] = Reads[Simple] { js =>
        (js \ "bar").validate[String].map(Simple(_))
      }
      // import import LoremCodec._ // Doesn't work in Scala 3.0.0-RC1
      implicit val loremReads: Reads[Lorem[Any]]     = LoremCodec.loremReads
      implicit val loremWrites: OWrites[Lorem[Any]]  = LoremCodec.loremWrites
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
      jsSimple.validate[Family].mustEqual(JsSuccess(simple))
      jsOptional.validate[Family].mustEqual(JsSuccess(optional))
      jsSimple.validate(Json.reads[Simple]).mustEqual(JsSuccess(simple))
      jsOptional.validate(Json.reads[Optional]).mustEqual(JsSuccess(optional))
    }

    "handle sealed family with custom discriminator name" in {
      implicit val cfg: JsonConfiguration = JsonConfiguration(discriminator = "_discriminator")
      implicit val simpleWrites: OWrites[Simple] = OWrites[Simple] { simple =>
        Json.obj("bar" -> simple.bar)
      }
      implicit val simpleReads: Reads[Simple] = Reads[Simple] { js =>
        (js \ "bar").validate[String].map(Simple(_))
      }
      // import import LoremCodec._ // Doesn't work in Scala 3.0.0-RC1
      implicit val loremReads: Reads[Lorem[Any]]     = LoremCodec.loremReads
      implicit val loremWrites: OWrites[Lorem[Any]]  = LoremCodec.loremWrites
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
      jsSimple.validate[Family].mustEqual(JsSuccess(simple))
      jsOptional.validate[Family].mustEqual(JsSuccess(optional))
      jsSimple.validate(Json.reads[Simple]).mustEqual(JsSuccess(simple))
      jsOptional.validate(Json.reads[Optional]).mustEqual(JsSuccess(optional))
    }

    "handle sealed family with typeNaming" in {
      implicit val cfg: JsonConfiguration = JsonConfiguration(typeNaming = JsonNaming {
        case "play.api.libs.json.MacroSpec.Simple"   => "simple"
        case "play.api.libs.json.MacroSpec.Lorem"    => "lorem"
        case "play.api.libs.json.MacroSpec.Optional" => "optional"
        case "Simple"                                => "simple"
        case "Lorem"                                 => "lorem"
        case "Optional"                              => "optional"
      })
      implicit val simpleWrites: OWrites[Simple] = OWrites[Simple] { simple =>
        Json.obj("bar" -> simple.bar)
      }
      implicit val simpleReads: Reads[Simple] = Reads[Simple] { js =>
        (js \ "bar").validate[String].map(Simple(_))
      }
      implicit val optionalFormat: OFormat[Optional] = Json.format[Optional]
      // import import LoremCodec._ // Doesn't work in Scala 3.0.0-RC1
      implicit val loremReads: Reads[Lorem[Any]]    = LoremCodec.loremReads
      implicit val loremWrites: OWrites[Lorem[Any]] = LoremCodec.loremWrites
      implicit val familyFormat: OFormat[Family]    = Json.format[Family]

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
      jsSimple.validate[Family].mustEqual(JsSuccess(simple))
      jsOptional.validate[Family].mustEqual(JsSuccess(optional))
      jsSimple.validate(Json.reads[Simple]).mustEqual(JsSuccess(simple))
      jsOptional.validate(Json.reads[Optional]).mustEqual(JsSuccess(optional))
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
  }
}

object MacroSpec {
  sealed trait Family
  case class Simple(bar: String)            extends Family
  case class Lorem[T](ipsum: T, age: Int)   extends Family
  case class Optional(prop: Option[String]) extends Family

  // lampepfl/dotty#11053 No support in inline/derivation for identifying generic subclasses
  object LoremCodec {
    implicit val loremReads: Reads[Lorem[Any]]    = Reads.failed("error.invalid")
    implicit val loremWrites: OWrites[Lorem[Any]] = OWrites(_ => JsObject.empty)
    implicit val loremFormat: OFormat[Lorem[Any]] = OFormat(loremReads, loremWrites)
  }

  sealed trait EmptyFamily

  object FamilyCodec {
    implicit val simpleWrites: OWrites[Simple]     = Json.writes[Simple]
    implicit val optionalWrites: OWrites[Optional] = Json.writes[Optional]
    import LoremCodec.loremWrites

    implicit val familyWrites: OWrites[Family] = Json.writes[Family] // Failing:
    /* java.lang.IllegalArgumentException:
     requirement failed: familyWrites  is not a valid identifier
     */
  }

  object Foo {
    // https://github.com/lampepfl/dotty/issues/11054 Type aliasing breaks constValue
    // import shapeless.tag.@@
    type Id = String // @@ Foo
    def id(value: String): Id = value.asInstanceOf[Id]

    implicit val idReads: Reads[Id] = implicitly[Reads[String]].asInstanceOf[Reads[Id]]
  }
  case class Foo(id: Foo.Id, value: Option[Either[String, Foo]])

  case class Interval[T](base: T, other: Option[T])
  case class Complex[T, U](id: Int, a: T, b: Either[T, String], c: U)

  type OptionalInt = Option[Int]
  case class UsingAlias(v: OptionalInt)

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
      "Json.writes[Family2]".mustNot(typeCheck)
      ???
    }
    /* Should fail, as there is no implicit for Family2Member,
     for now due to the contravariance `w` being defined is self resolved
     as Writes instance this subtype Family2Member
     */
  }
}
