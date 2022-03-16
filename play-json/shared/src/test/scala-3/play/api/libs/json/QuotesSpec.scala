/*
 * Copyright (C) 2009-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import scala.deriving.Mirror

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

final class QuotesSpec extends AnyWordSpec with Matchers with org.scalatestplus.scalacheck.ScalaCheckPropertyChecks:
  import TestMacros._

  "Product" should {
    "be inspected for elements" when {
      "Foo" in {
        testProductElements[Foo].mustEqual(
          List(
            "(val bar,List(),TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class scala)),object Predef),type String))",
            "(val lorem,List(),TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class <root>)),object scala),class Int))"
          )
        )
      }

      "generic Bar" in {
        testProductElements[Bar[Int]].mustEqual(
          List(
            "(val name,List(),TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class scala)),object Predef),type String))",
            "(val opt,List(),AppliedType(TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class <root>)),object scala),class Option),List(TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class <root>)),object scala),class Int))))",
            "(val scores,List(),AppliedType(TypeRef(ThisType(TypeRef(NoPrefix,module class immutable)),trait Seq),List(TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class <root>)),object scala),class Double))))"
          )
        )
      }

      "of non-case class" when {
        "there is no ProductOf" in {
          testProductElements[TestUnion.UC].size.mustEqual(0)
        }

        "it's defined a ill-typed ProductOf" in {
          // Bad refinement type, so element labels/types cannot be resolved
          given pof: Mirror.ProductOf[TestUnion.UC] = TestUnion.ProductOfUC

          testProductElements[TestUnion.UC].size.mustEqual(0)
        }

        "it's defined a well-typed ProductOf" when {
          val expected = List(
            "(val name,List(),TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class scala)),object Predef),type String))",
            "(val age,List(),TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class <root>)),object scala),class Int))"
          )

          "by import" in {
            import TestUnion.Implicits.productOfUC

            testProductElements[TestUnion.UC].mustEqual(expected)
          }

          "by local val" in {
            implicit val pof = TestUnion.ProductOfUC

            testProductElements[TestUnion.UC].mustEqual(expected)
          }
        }
      }
    }

    "be created" when {
      "from Foo" in {
        testWithTuple(
          Foo("1", 2)
        ) mustEqual "scala.Tuple2[scala.Predef.String, scala.Int]/Foo(1,2)"
      }

      "from generic Bar" in {
        testWithTuple(
          Bar[Double]("bar1", None, Seq(1.2D, 34.5D))
        ).mustEqual(
          "scala.Tuple3[scala.Predef.String, scala.Option[scala.Double], scala.collection.immutable.Seq[scala.Double]]/Bar(bar1,None,List(1.2, 34.5))"
        )

        testWithTuple(
          Bar[Int]("bar2", Some(2), Seq(3.45D))
        ).mustEqual(
          "scala.Tuple3[scala.Predef.String, scala.Option[scala.Int], scala.collection.immutable.Seq[scala.Double]]/Bar(bar2,Some(2),List(3.45))"
        )
      }

      "from BigFat" in {
        val tooBig = BigFat(
          a = 1,
          b = 2D,
          c = 3F,
          d = "d",
          e = Seq(1, 2, 3),
          f = 6,
          g = 7D,
          h = 8F,
          i = "i",
          j = Seq(4, 5),
          k = 10,
          l = 11D,
          m = 12F,
          n = "n",
          o = Seq(6, 7),
          p = 13,
          q = 14D,
          r = 15F,
          s = "s",
          t = Seq(8),
          u = 16F,
          v = "v",
          w = Seq(9, 10, 11),
          x = 12,
          y = Seq(13, 14),
          z = 15D
        )

        testWithTuple[BigFat](tooBig).mustEqual("TODO")
      }

      "from non-case class" when {
        "fail when there is no Conversion[T, _ <: Product]" in {
          """testWithTuple(new TestUnion.UC("name", 2))""".mustNot(typeCheck)
        }

        "fail when Conversion[T, _ <: Product] defined without ProductOf" in {
          implicit val conv = TestUnion.ucAsProduct

          testWithTuple(
            new TestUnion.UC("name", 2)
          ).mustEqual("scala.Tuple$package.EmptyTuple/(name,2)")
        }

        "be successful when conversion is provided" in {
          import TestUnion.Implicits.productOfUC
          implicit val conv = TestUnion.ucAsProduct

          testWithTuple(
            new TestUnion.UC("name", 2)
          ).mustEqual("scala.Tuple2[scala.Predef.String, scala.Int]/(name,2)")
        }
      }
    }

    "be transformed" when {
      "Foo" in {
        testWithFields(Foo("3", 4)).mustEqual("bar=3,lorem=4")
      }

      "generic Bar" in {
        testWithFields(
          Bar("bar3", Some("opt2"), Seq(3.1D, 4.5D))
        ).mustEqual("name=bar3,opt=Some(opt2),scores=List(3.1, 4.5)")
      }
    }
  }

  "Direct known subtypes" should {
    "be resolved for sealed trait" in {
      testKnownSubtypes[TestUnion.UT].mustEqual(
        List(
          "play.api.libs.json.TestUnion.UA",
          "play.api.libs.json.TestUnion.UB",
          "play.api.libs.json.TestUnion.UC",
          "play.api.libs.json.TestUnion.UD",
          "play.api.libs.json.TestUnion.UE" // through UTT sub-trait
        )
      )
    }
  }
end QuotesSpec

case class Foo(bar: String, lorem: Int)

case class Bar[T](name: String, opt: Option[T], scores: Seq[Double])

case class BigFat(
  a: Int,
  b: Double,
  c: Float,
  d: String,
  e: Seq[Int],
  f: Int,
  g: Double,
  h: Float,
  i: String,
  j: Seq[Int],
  k: Int,
  l: Double,
  m: Float,
  n: String,
  o: Seq[Int],
  p: Int,
  q: Double,
  r: Float,
  s: String,
  t: Seq[Int],
  u: Float,
  v: String,
  w: Seq[Int],
  x: Int,
  y: Seq[Int],
  z: Double)

object TestUnion:
  sealed trait UT
  case object UA              extends UT
  case class UB(name: String) extends UT

  class UC(val name: String, @CustomAnnot val age: Int) extends UT

  object UD extends UT

  sealed trait UTT extends UT
  case class UE()  extends UTT

  object ProductOfUC extends Mirror.Product {
    type MirroredType       = TestUnion.UC
    type MirroredElemTypes  = Tuple2[String, Int]
    type MirroredMonoType   = TestUnion.UC
    type MirroredLabel      = "UC"
    type MirroredElemLabels = Tuple2["name", "age"]

    def fromProduct(p: Product): MirroredMonoType =
      new TestUnion.UC(
        p.productElement(0).asInstanceOf[String],
        p.productElement(1).asInstanceOf[Int]
      )
  }

  object Implicits:
    implicit val productOfUC: ProductOfUC.type = ProductOfUC
  end Implicits

  val ucAsProduct: Conversion[TestUnion.UC, Tuple2[String, Int]] =
    (uc: TestUnion.UC) => uc.name -> uc.age

  import scala.annotation.{ meta, StaticAnnotation }

  @meta.field
  final class CustomAnnot extends StaticAnnotation:
    override def hashCode: Int = 1667526726

    override def equals(that: Any): Boolean = that match {
      case _: this.type => true
      case _            => false
    }
  end CustomAnnot
end TestUnion
