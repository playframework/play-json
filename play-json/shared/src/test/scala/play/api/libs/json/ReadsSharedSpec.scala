/*
 * Copyright (C) 2009-2019 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import java.math.BigInteger

import java.util.Locale

import org.scalatest._

class ReadsSharedSpec extends WordSpec with MustMatchers {
  "Reads" should {
    "not repath the second result on flatMap" when {
      val aPath = JsPath \ "a"
      val readsA: Reads[String] = aPath.read[String]
      val value = "string"
      val aJson = aPath.write[String].writes(value)

      "in case of success" in {
        val flatMappedReads = readsA.flatMap(_ => readsA)
        aJson.validate(flatMappedReads) mustEqual JsSuccess(value, aPath)
      }

      "in case of failure" in {
        val readsAFail = aPath.read[Int]
        val flatMappedReads = readsA.flatMap(_ => readsAFail)

        aJson.validate(flatMappedReads).
          mustEqual(JsError(List((aPath, List(
            JsonValidationError("error.expected.jsnumber")
          )))))
      }
    }

    "widen" in {
      // !! Keep type ascriptions
      val orig: Reads[List[String]] = implicitly[Reads[List[String]]]
      val widened: Reads[Traversable[String]] = orig.widen

      widened.reads(JsArray(Seq(JsString("foo"), JsString("bar")))).
        mustEqual(JsSuccess[Traversable[String]](List("foo", "bar")))

    }

    "be failed" in {
      val r: Reads[String] = Reads.failed[String]("Foo")

      r.reads(Json.obj()) mustEqual JsError("Foo")
    }
  }

  "Map" should {
    "be successfully read with string keys" in {
      Json.fromJson[Map[String, Int]](
        Json.obj("foo" -> 1, "bar" -> 2)) mustEqual (
          JsSuccess(Map("foo" -> 1, "bar" -> 2)))
    }

    "be read with character keys" which {
      "are characters" in {
        Json.fromJson[Map[Char, Int]](Json.obj("a" -> 1, "b" -> 2))(
          Reads.charMapReads) mustEqual JsSuccess(Map('a' -> 1, 'b' -> 2))
      }

      "are not characters" in {
        Json.fromJson[Map[Char, Int]](Json.obj("foo" -> 1, "bar" -> 2))(
          Reads.charMapReads) mustEqual JsError(List(
            (JsPath \ "foo", List(JsonValidationError("error.invalid.character"))),
            (JsPath \ "bar", List(JsonValidationError("error.invalid.character")))
          ))
      }
    }
  }

  "Compose" should {
    lazy val generated: Reads[Owner] = Json.reads

    "preprocess a JSON object using a Reads" in {
      implicit val reads: Reads[Owner] =
        generated.composeWith(Reads[JsValue] {
          case obj @ JsObject(_) => (obj \ "avatar").asOpt[String] match {
            case Some(_) => JsSuccess(obj)
            case _ => JsSuccess(obj + ("avatar" -> JsString("")))
          }

          case _ => JsError("Object expected")
        })

      Json.obj("login" -> "foo", "url" -> "url://id").
        validate[Owner] mustEqual JsSuccess(Owner("foo", "", "url://id"))

    }

    "preprocess a JSON object using a function" in {
      implicit val reads: Reads[Owner] = generated.preprocess {
        case obj @ JsObject(_) => (obj \ "avatar").asOpt[String] match {
          case Some(_) => obj
          case _ => obj + ("avatar" -> JsString(""))
        }
      }

      Json.obj("login" -> "foo", "url" -> "url://id").
        validate[Owner] mustEqual JsSuccess(Owner("foo", "", "url://id"))

    }
  }

  "Functional Reads" should {
    import play.api.libs.functional.syntax._

    implicit val reads: Reads[Owner] = (
      (__ \ "login").read[String] and
      (__ \ "avatar").read[String] and
      (__ \ "url").read[String]
    )(Owner)

    "be successful for simple case class Owner" in {
      val jsObj = Json.obj(
        "login" -> "foo",
        "avatar" -> "url://avatar",
        "url" -> "url://id"
      )

      Json.parse(Json.stringify(jsObj)) mustEqual jsObj
    }
  }

  "Big integer Reads" should {
    Seq("123", "23").foreach { repr =>
      val jb = new BigInteger(repr)
      val sb = BigInt(jb)

      s"""be successful for JsString("$repr")""" in {
        val jsStr = JsString(repr)

        jsStr.validate[BigInteger] mustEqual JsSuccess(jb)
        jsStr.validate[BigInt] mustEqual JsSuccess(sb)
      }

      s"""be successful for JsNumber($sb)""" in {
        val jsNum = JsNumber(BigDecimal(sb))

        jsNum.validate[BigInteger] mustEqual JsSuccess(jb)
        jsNum.validate[BigInt] mustEqual JsSuccess(sb)
      }
    }

    Seq("1.0", "A").foreach { repr =>
      s"fails for '$repr'" in {
        val jsStr = JsString(repr)
        val jsErr = JsError(List((JsPath, List(
          JsonValidationError("error.expected.numberformatexception")
        ))))

        jsStr.validate[BigInteger] mustEqual jsErr
      }
    }
  }

  "EnumFormat" should {
    import TestEnums.EnumWithCustomNames._
    import TestEnums.EnumWithDefaultNames._

    "deserialize correctly enum with custom names" in {
      JsString("ENUM1").validate[EnumWithCustomNames] mustEqual JsSuccess(customEnum1)
      JsString("ENUM2").validate[EnumWithCustomNames] mustEqual JsSuccess(customEnum2)
    }

    "deserialize correctly enum with default names" in {
      JsString("defaultEnum1").validate[EnumWithDefaultNames] mustEqual JsSuccess(defaultEnum1)
      JsString("defaultEnum2").validate[EnumWithDefaultNames] mustEqual JsSuccess(defaultEnum2)
    }
  }

  // ---

  case class Owner(
    login: String,
    avatar: String,
    url: String
  )
}
