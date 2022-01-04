/*
 * Copyright (C) 2009-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import java.math.BigInteger

import java.net.URI

import org.scalatest._
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

final class ReadsSharedSpec extends AnyWordSpec with Matchers with Inside {
  "Reads" should {
    "not repath the second result on flatMap" when {
      val aPath                 = JsPath \ "a"
      val readsA: Reads[String] = aPath.read[String]
      val value                 = "string"
      val aJson                 = aPath.write[String].writes(value)

      "in case of success" in {
        val flatMappedReads = readsA.flatMap(_ => readsA)
        aJson.validate(flatMappedReads).mustEqual(JsSuccess(value, aPath))
      }

      "in case of failure" in {
        val readsAFail      = aPath.read[Int]
        val flatMappedReads = readsA.flatMap(_ => readsAFail)

        aJson
          .validate(flatMappedReads)
          .mustEqual(
            JsError(
              List(
                (
                  aPath,
                  List(
                    JsonValidationError("error.expected.jsnumber")
                  )
                )
              )
            )
          )
      }
    }

    "widen" in {
      // !! Keep type ascriptions
      val orig: Reads[List[String]]           = implicitly[Reads[List[String]]]
      val widened: Reads[Traversable[String]] = orig.widen

      widened
        .reads(JsArray(Seq(JsString("foo"), JsString("bar"))))
        .mustEqual(JsSuccess[Traversable[String]](List("foo", "bar")))
    }

    "be failed" in {
      val r: Reads[String] = Reads.failed[String]("Foo")

      r.reads(Json.obj()).mustEqual(JsError("Foo"))
    }
  }

  "Map" should {
    "be successfully read with string keys" in {
      Json
        .fromJson[Map[String, Int]](Json.obj("foo" -> 1, "bar" -> 2))
        .mustEqual(JsSuccess(Map("foo" -> 1, "bar" -> 2)))
    }

    "be read with character keys".which {
      "are characters" in {
        Json
          .fromJson[Map[Char, Int]](Json.obj("a" -> 1, "b" -> 2))
          .mustEqual(JsSuccess(Map('a' -> 1, 'b' -> 2)))
      }

      "are not characters" in {
        Json
          .fromJson[Map[Char, Int]](Json.obj("foo" -> 1, "bar" -> 2))(Reads.charMapReads)
          .mustEqual(
            JsError(
              List(
                (JsPath \ "foo", List(JsonValidationError("error.invalid.character"))),
                (JsPath \ "bar", List(JsonValidationError("error.invalid.character")))
              )
            )
          )
      }
    }

    "be read with boolean keys".which {
      "are booleans" in {
        Json
          .fromJson[Map[Boolean, String]](Json.obj("true" -> "foo", "false" -> "bar"))
          .mustEqual(JsSuccess(Map[Boolean, String](true -> "foo", false -> "bar")))
      }

      "are not booleans" in {
        Json
          .fromJson[Map[Boolean, String]](Json.obj("foo" -> "", "bar" -> ""))
          .mustEqual(
            JsError(
              List(
                (JsPath \ "foo", List(JsonValidationError("error.expected.boolean"))),
                (JsPath \ "bar", List(JsonValidationError("error.expected.boolean")))
              )
            )
          )
      }
    }

    "be read with byte keys" in {
      Json
        .fromJson[Map[Byte, Int]](Json.obj("a" -> 1, "b" -> 2))
        .mustEqual(JsSuccess(Map('a'.toByte -> 1, 'b'.toByte -> 2)))
    }

    "be read with short keys" in {
      Json
        .fromJson[Map[Short, Int]](Json.obj("1" -> 1, "2" -> 2))
        .mustEqual(JsSuccess(Map(1.toByte -> 1, 2.toByte -> 2)))
    }

    "be read with int keys" in {
      Json
        .fromJson[Map[Int, String]](Json.obj("1" -> "foo", "2" -> "bar"))
        .mustEqual(JsSuccess(Map(1 -> "foo", 2 -> "bar")))
    }

    "be read with long keys" in {
      Json
        .fromJson[Map[Long, String]](Json.obj("1" -> "foo", "2" -> "bar"))
        .mustEqual(JsSuccess(Map(1L -> "foo", 2L -> "bar")))
    }

    "be read with float keys" in {
      Json
        .fromJson[Map[Float, String]](Json.obj("1.23" -> "foo", "23.4" -> "bar"))
        .mustEqual(JsSuccess(Map(1.23F -> "foo", 23.4F -> "bar")))
    }

    "be read with double keys" in {
      Json
        .fromJson[Map[Double, String]](Json.obj("1.23" -> "foo", "23.4" -> "bar"))
        .mustEqual(JsSuccess(Map(1.23D -> "foo", 23.4D -> "bar")))
    }

    "be read with Reads'able keys" in {
      val key = "https://www.playframework.com/documentation/2.8.x/api/scala/index.html#play.api.libs.json.JsResult"

      implicitly[KeyReads[URI]]

      Json.fromJson[Map[URI, String]](Json.obj(key -> "foo")).mustEqual(JsSuccess(Map((new URI(key)) -> "foo")))
    }
  }

  "Compose" should {
    lazy val generated: Reads[Owner] = Json.reads

    "preprocess a JSON object using a Reads" in {
      implicit val reads: Reads[Owner] =
        generated.composeWith(Reads[JsValue] {
          case obj @ JsObject(_) =>
            (obj \ "avatar").asOpt[String] match {
              case Some(_) => JsSuccess(obj)
              case _       => JsSuccess(obj + ("avatar" -> JsString("")))
            }

          case _ => JsError("Object expected")
        })

      Json.obj("login" -> "foo", "url" -> "url://id").validate[Owner].mustEqual(JsSuccess(Owner("foo", "", "url://id")))
    }

    "preprocess a JSON object using a function" in {
      implicit val reads: Reads[Owner] = generated.preprocess {
        case obj @ JsObject(_) =>
          (obj \ "avatar").asOpt[String] match {
            case Some(_) => obj
            case _       => obj + ("avatar" -> JsString(""))
          }
      }

      Json.obj("login" -> "foo", "url" -> "url://id").validate[Owner].mustEqual(JsSuccess(Owner("foo", "", "url://id")))
    }
  }

  "Reads result" should {
    "be flat-mapped" in {
      val readsArrayAsOwner: Reads[Owner] =
        Reads.seq[String].flatMapResult {
          case login +: avatar +: url +: _ =>
            JsSuccess(Owner(login, avatar, url))

          case _ =>
            JsError("error.expected.owner-as-jsarray")
        }

      readsArrayAsOwner
        .reads(JsArray(Seq(JsString("foo"), JsString("bar"), JsString("url://owner"))))
        .mustEqual(
          JsSuccess(
            Owner("foo", "bar", "url://owner")
          )
        )
    }
  }

  "Functional Reads" should {
    import play.api.libs.functional.syntax._

    implicit val reads: Reads[Owner] = (
      (__ \ "login").read[String] and
        (__ \ "avatar").read[String] and
        (__ \ "url").read[String]
    )(Owner.apply _)

    "be successful for simple case class Owner" in {
      val jsObj = Json.obj(
        "login"  -> "foo",
        "avatar" -> "url://avatar",
        "url"    -> "url://id"
      )

      Json.parse(Json.stringify(jsObj)).mustEqual(jsObj)

      jsObj.validate[Owner].mustEqual(JsSuccess(Owner(login = "foo", avatar = "url://avatar", url = "url://id")))
    }
  }

  "BigDecimal Reads" should {
    Seq("123", "23", "1.23", "1E+1").foreach { repr =>
      s"""be successful for JsString("$repr")""" in {
        val jsStr = JsString(repr)
        jsStr.validate[BigDecimal].mustEqual(JsSuccess(BigDecimal(repr)))
        jsStr.validate[java.math.BigDecimal].mustEqual(JsSuccess(new java.math.BigDecimal(repr)))
      }
    }

    Seq("1..0", "A").foreach { repr =>
      s"fail for '$repr'" in {
        val jsStr   = JsString(repr)
        val jsError = JsError("error.expected.numberformatexception")
        jsStr.validate[BigDecimal].mustEqual(jsError)
        jsStr.validate[java.math.BigDecimal].mustEqual(jsError)
      }
    }
  }

  "BigInteger Reads" should {
    Seq("123", "23").foreach { repr =>
      val jb = new BigInteger(repr)
      val sb = BigInt(jb)

      s"""be successful for JsString("$repr")""" in {
        val jsStr = JsString(repr)

        jsStr.validate[BigInteger].mustEqual(JsSuccess(jb))
        jsStr.validate[BigInt].mustEqual(JsSuccess(sb))
      }

      s"""be successful for JsNumber($sb)""" in {
        val jsNum = JsNumber(BigDecimal(sb))

        jsNum.validate[BigInteger].mustEqual(JsSuccess(jb))
        jsNum.validate[BigInt].mustEqual(JsSuccess(sb))
      }
    }

    Seq("1.0", "A").foreach { repr =>
      s"fails for '$repr'" in {
        JsString(repr).validate[BigInteger].mustEqual(JsError("error.expected.numberformatexception"))
      }
    }
  }

  "EnumFormat" should {
    import TestEnums.EnumWithCustomNames._

    "deserialize correctly enum with custom names" in {
      JsString("ENUM1").validate[EnumWithCustomNames].mustEqual(JsSuccess(customEnum1))
      JsString("ENUM2").validate[EnumWithCustomNames].mustEqual(JsSuccess(customEnum2))
    }
  }

  "URI" should {
    "be read from JsString" in {
      val strRepr = "https://www.playframework.com/documentation/2.8.x/api/scala/index.html#play.api.libs.json.JsResult"

      JsString(strRepr).validate[URI].mustEqual(JsSuccess(new URI(strRepr)))
    }

    "not be read from invalid JsString" in {
      val strRepr = " invalid"

      inside(JsString(strRepr).validate[URI]) {
        case JsError.Message(msg) =>
          msg.must(include("invalid"))
      }
    }
  }

  "Identity reads" should {
    def success[T <: JsValue](fixture: T)(implicit r: Reads[T], ct: scala.reflect.ClassTag[T]) =
      s"be resolved for $fixture as ${ct.runtimeClass.getSimpleName}" in {
        r.reads(fixture).mustEqual(JsSuccess(fixture))
      }

    success[JsArray](Json.arr("foo", 2))
    success[JsValue](Json.arr("foo", 2))

    success[JsBoolean](JsFalse)
    success[JsValue](JsTrue)

    success[JsNull.type](JsNull)
    success[JsValue](JsNull)

    success[JsNumber](JsNumber(1))
    success[JsValue](JsNumber(1))

    success[JsObject](JsObject(Map("foo" -> JsNumber(1))))
    success[JsValue](JsObject(Map("foo" -> JsNumber(1))))

    success[JsString](JsString("foo"))
    success[JsValue](JsString("foo"))
  }

  // ---

  case class Owner(
      login: String,
      avatar: String,
      url: String
  )
}
