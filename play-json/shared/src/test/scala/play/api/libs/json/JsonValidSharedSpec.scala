/*
 * Copyright (C) 2009-2018 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import play.api.libs.json._
import play.api.libs.json.Json._
import play.api.libs.functional.syntax._

import org.scalatest._

class JsonValidSharedSpec extends WordSpec with MustMatchers {
  "JSON reads" should {
    "validate simple types" in {
      JsString("string").validate[String] mustEqual (JsSuccess("string"))
      JsNumber(5).validate[Int] mustEqual (JsSuccess(5))
      JsNumber(5L).validate[Long] mustEqual (JsSuccess(5L))
      JsNumber(5).validate[Short] mustEqual (JsSuccess(5))
      JsNumber(123.5).validate[Float] mustEqual (JsSuccess(123.5))
      JsNumber(123456789123456.56).validate[Double] mustEqual (JsSuccess(123456789123456.56))
      JsBoolean(true).validate[Boolean] mustEqual (JsSuccess(true))
      JsTrue.validate[Boolean] mustEqual (JsSuccess(true))
      JsFalse.validate[Boolean] mustEqual (JsSuccess(false))
      JsString("123456789123456.56").validate[BigDecimal] mustEqual (JsSuccess(BigDecimal(123456789123456.56)))
      JsNumber(123456789123456.56).validate[BigDecimal] mustEqual (JsSuccess(BigDecimal(123456789123456.567891234)))
      JsNumber(123456789.56).validate[java.math.BigDecimal] mustEqual (JsSuccess(new java.math.BigDecimal("123456789.56")))
      JsString("123456789123456.56").validate[java.math.BigDecimal] mustEqual (JsSuccess(new java.math.BigDecimal("123456789123456.56")))
    }

    "invalidate wrong simple type conversion" in {
      JsString("string").validate[Long] mustEqual (JsError(Seq(JsPath() -> Seq(JsonValidationError("error.expected.jsnumber")))))
      JsNumber(5).validate[String] mustEqual (JsError(Seq(JsPath() -> Seq(JsonValidationError("error.expected.jsstring")))))
      JsNumber(5.123).validate[Int] mustEqual (JsError(Seq(JsPath() -> Seq(JsonValidationError("error.expected.int")))))
      JsNumber(300).validate[Byte] mustEqual (JsError(Seq(JsPath() -> Seq(JsonValidationError("error.expected.byte")))))
      JsNumber(Long.MaxValue).validate[Int] mustEqual (JsError(Seq(JsPath() -> Seq(JsonValidationError("error.expected.int")))))
      JsBoolean(false).validate[Double] mustEqual (JsError(Seq(JsPath() -> Seq(JsonValidationError("error.expected.jsnumber")))))
    }

    "validate simple numbered type conversion" in {
      JsNumber(5).validate[Double] mustEqual (JsSuccess(5.0))
      JsNumber(BigDecimal(5)).validate[Double] mustEqual (JsSuccess(5.0))
      JsNumber(5.123).validate[BigDecimal] mustEqual (JsSuccess(BigDecimal(5.123)))
    }

    "return JsResult with correct values for isSuccess and isError" in {
      JsString("s").validate[String].isSuccess mustEqual (true)
      JsString("s").validate[String].isError mustEqual (false)
      JsString("s").validate[Long].isSuccess mustEqual (false)
      JsString("s").validate[Long].isError mustEqual (true)
    }

    "validate JsObject to Map" in {
      Json.obj("key1" -> "value1", "key2" -> "value2").validate[Map[String, String]] mustEqual (JsSuccess(Map("key1" -> "value1", "key2" -> "value2")))
      Json.obj("key1" -> 5, "key2" -> 3).validate[Map[String, Int]] mustEqual (JsSuccess(Map("key1" -> 5, "key2" -> 3)))
      Json.obj("key1" -> 5.123, "key2" -> 3.543).validate[Map[String, Double]] mustEqual (JsSuccess(Map("key1" -> 5.123, "key2" -> 3.543)))
    }

    "invalidate JsObject to Map with wrong type conversion" in {
      Json.obj("key1" -> "value1", "key2" -> "value2", "key3" -> "value3").validate[Map[String, Int]] mustEqual (
        JsError(Seq(
          JsPath \ "key1" -> Seq(JsonValidationError("error.expected.jsnumber")),
          JsPath \ "key2" -> Seq(JsonValidationError("error.expected.jsnumber")),
          JsPath \ "key3" -> Seq(JsonValidationError("error.expected.jsnumber"))
        ))
      )

      Json.obj("key1" -> "value1", "key2" -> 5, "key3" -> true).validate[Map[String, Int]] mustEqual (
        JsError(Seq(
          JsPath \ "key1" -> Seq(JsonValidationError("error.expected.jsnumber")),
          JsPath \ "key3" -> Seq(JsonValidationError("error.expected.jsnumber"))
        ))
      )
    }

    "validate JsArray to List" in {
      Json.arr("alpha", "beta", "delta").validate[List[String]] mustEqual (JsSuccess(List("alpha", "beta", "delta")))
      Json.arr(123, 567, 890).validate[List[Int]] mustEqual (JsSuccess(List(123, 567, 890)))
      Json.arr(123.456, 567.123, 890.654).validate[List[Double]] mustEqual (JsSuccess(List(123.456, 567.123, 890.654)))
    }

    "invalidate JsArray to List with wrong type conversion" in {
      Json.arr(123.456, 567.123, 890.654).validate[List[Int]] mustEqual (
        JsError(Seq(
          JsPath(0) -> Seq(JsonValidationError("error.expected.int")),
          JsPath(1) -> Seq(JsonValidationError("error.expected.int")),
          JsPath(2) -> Seq(JsonValidationError("error.expected.int"))
        ))
      )
      Json.arr("alpha", "beta", "delta").validate[List[Int]] mustEqual (
        JsError(Seq(
          JsPath(0) -> Seq(JsonValidationError("error.expected.jsnumber")),
          JsPath(1) -> Seq(JsonValidationError("error.expected.jsnumber")),
          JsPath(2) -> Seq(JsonValidationError("error.expected.jsnumber"))
        ))
      )

      Json.arr("alpha", 5, true).validate[List[Int]] mustEqual (
        JsError(Seq(
          JsPath(0) -> Seq(JsonValidationError("error.expected.jsnumber")),
          JsPath(2) -> Seq(JsonValidationError("error.expected.jsnumber"))
        ))
      )
    }
    "validate JsArray of stream to List" in {
      JsArray(Stream("alpha", "beta", "delta") map JsString.apply).validate[List[String]] mustEqual (JsSuccess(List("alpha", "beta", "delta")))
    }

    "invalidate JsArray of stream to List with wrong type conversion" in {
      JsArray(Stream(JsNumber(1), JsString("beta"), JsString("delta"), JsNumber(4), JsString("five"))).validate[List[Int]] mustEqual (
        JsError(Seq(
          JsPath(1) -> Seq(JsonValidationError("error.expected.jsnumber")),
          JsPath(2) -> Seq(JsonValidationError("error.expected.jsnumber")),
          JsPath(4) -> Seq(JsonValidationError("error.expected.jsnumber"))
        ))
      )

      JsArray(Stream(JsString("alpha"), JsNumber(5), JsBoolean(true))).validate[List[Int]] mustEqual (
        JsError(Seq(
          JsPath(0) -> Seq(JsonValidationError("error.expected.jsnumber")),
          JsPath(2) -> Seq(JsonValidationError("error.expected.jsnumber"))
        ))
      )
    }

    "validate UUID" when {
      "there is a correct UUID" in {
        val uuid = java.util.UUID.randomUUID()
        Json.toJson[java.util.UUID](uuid).validate[java.util.UUID] mustEqual (JsSuccess(uuid))
      }

      "reject malformed UUIDs" in {
        JsString("bogus string").validate[java.util.UUID].recoverTotal {
          e => "error"
        } mustEqual ("error")
      }

      "reject well-formed but incorrect UUIDS in strict mode" in {
        JsString("0-0-0-0-0").validate[java.util.UUID](new Reads.UUIDReader(true)).recoverTotal {
          e => "error"
        } mustEqual ("error")
      }
    }

    "validate Enums" in {
      object Weekdays extends Enumeration {
        val Mon, Tue, Wed, Thu, Fri, Sat, Sun = Value
      }
      val json = Json.obj("day1" -> Weekdays.Mon, "day2" -> "tue", "day3" -> 3)

      (json.validate((__ \ "day1").read(Reads.enumNameReads(Weekdays))).asOpt mustEqual Some(Weekdays.Mon))
      (json.validate((__ \ "day2").read(Reads.enumNameReads(Weekdays))).asOpt mustEqual None)
      (json.validate((__ \ "day3").read(Reads.enumNameReads(Weekdays))).asOpt mustEqual None)
    }

    "read fields with null values" in {
      val json = Json.obj("field" -> JsNull)

      val resultPost = json.validate((__ \ "field").read(Reads.optionWithNull[String]))
      resultPost.get mustEqual (None)
    }

    "validate options using validateOpt" in {
      val json = Json.obj("foo" -> JsNull, "bar" -> "bar")

      (json \ "foo").validateOpt[String] mustEqual JsSuccess(None)
      (json \ "bar").validateOpt[Int] mustEqual JsError("error.expected.jsnumber")
      (json \ "bar").validateOpt[String] mustEqual JsSuccess(Some("bar"))
      (json \ "baz").validateOpt[String] mustEqual JsSuccess(None)
    }
  }

  "JSON JsResult" should {
    "recover from error" in {
      JsNumber(123).validate[String].recover {
        case JsError(e) => "error"
      } mustEqual JsSuccess("error")

      JsNumber(123).validate[String].recoverTotal {
        e => "error"
      } mustEqual "error"

      JsNumber(123).validate[Int].recoverTotal {
        e => 0
      } mustEqual 123
    }
  }

  "JSON case class/tuple validation" should {
    case class User(name: String, age: Int)

    "validate simple reads" in {
      JsString("alphabeta").validate[String] mustEqual (JsSuccess("alphabeta"))
    }

    "validate reads on the root path" when {
      case class Address(street: String, zip: String)

      implicit val userReads = (
        (__ \ "name").read[String] and
        (__ \ "age").read[Int]
      )(User)

      implicit val addressReads = (
        (__ \ "street").read[String] and
        (__ \ "zip").read[String]
      )(Address)

      val bobby = Json.obj(
        "name" -> "bobby",
        "age" -> 54,
        "street" -> "13 Main St",
        "zip" -> "98765"
      )

      "reads" in {
        implicit val userAddressReads: Reads[(User, Address)] = (
          __.read[User] and
          __.read[Address]
        ).tupled

        bobby.validate[(User, Address)] mustEqual (
          JsSuccess((User("bobby", 54), Address("13 Main St", "98765")))
        )
      }

      "readNullables" in {
        implicit val userAddressReads: Reads[(User, Option[Address])] = (
          __.read[User] and
          __.readNullable[Address]
        ).tupled

        bobby.validate[(User, Option[Address])] mustEqual (
          JsSuccess((User("bobby", 54), Some(Address("13 Main St", "98765"))))
        )
      }

      "readNullables for missing root path fragment" in {
        implicit val userAddressReads = (
          __.read[User] and
          __.readNullable[Address]
        ).tupled

        val missingAddressBobby = Json.obj(
          "name" -> "bobby",
          "age" -> 54
        )

        missingAddressBobby.validate(userAddressReads) mustEqual (
          JsError(__ \ "street", JsonValidationError("error.path.missing")) ++
          JsError(__ \ "zip", JsonValidationError("error.path.missing"))
        )
      }

      "readNullables for badly formed root path" in {
        implicit val userAddressReads: Reads[(User, Option[Address])] = (
          __.read[User] and
          __.readNullable[Address]
        ).tupled

        val missingZipBobby = Json.obj(
          "name" -> "bobby",
          "age" -> 54,
          "street" -> "13 Main St"
        )

        missingZipBobby.validate(userAddressReads) mustEqual (
          JsError(__ \ "zip", JsonValidationError("error.path.missing"))
        )
      }

      "readNullables for null root path" in {
        implicit val userAddressReads = (
          __.readNullable[User] and
          __.readNullable[Address]
        ).tupled

        JsNull.validate(userAddressReads) mustEqual (
          JsSuccess(None -> None)
        )
      }
    }

    "validate simple constraints" in {
      JsString("alphabeta").validate[String](Reads.minLength(5)) mustEqual (JsSuccess("alphabeta"))
    }

    "test JsPath.create" in {
      JsPath.createObj(
        JsPath \ "toto" \ "toto1" -> JsString("alpha"),
        JsPath \ "titi" \ "titi1" -> JsString("beta"),
        JsPath \ "titi" \ "titi2" -> JsString("beta2")
      )
    }

    "validate simple case class reads/writes" in {
      val bobby = User("bobby", 54)

      implicit val userReads = {
        import Reads.path._
        (
          at(JsPath \ "name")(Reads.minLength[String](5))
          and
          at(JsPath \ "age")(Reads.min(40))
        )(User)
      }

      implicit val userWrites = {
        import Writes.path._
        (
          at[String](JsPath \ "name")
          and
          at[Int](JsPath \ "age")
        )(unlift(User.unapply))
      }

      val js = Json.toJson(bobby)

      js.validate[User] mustEqual (JsSuccess(bobby))
    }

    "validate simple case class format with custom apply/unapply" in {
      val bobby = User("bobby", 54)

      implicit val userFormats = {
        import Format.path._; import Format.constraints.{ of => o, _ }
        (
          at(JsPath \ "name")(Format(Reads.minLength[String](5), o[String]))
          and
          at(JsPath \ "age")(Format(Reads.min(40), o[Int]))
        )(User, unlift(User.unapply))
      }

      val js = Json.toJson(bobby)

      js.validate[User] mustEqual (JsSuccess(bobby))
    }

    "validate simple case class format" in {
      val bobby = User("bobby", 54)

      implicit val userFormats = {
        import Format.constraints.{ of => o, _ }
        (
          (__ \ "name").rw(Reads.minLength[String](5), o[String])
          and
          (__ \ "age").rw(Reads.min(40), o[Int])
        ) apply (User, unlift(User.unapply))
      }

      val js = Json.toJson(bobby)

      js.validate[User] mustEqual (JsSuccess(bobby))
    }

    "JsObject tupled reads" in {
      implicit val dataReads: Reads[(String, Int)] = {
        import Reads.path._
        (
          at[String](__ \ "uuid") and
          at[Int](__ \ "nb")
        ).tupled
      }

      val js = Json.obj(
        "uuid" -> "550e8400-e29b-41d4-a716-446655440000",
        "nb" -> 654
      )

      js.validate[(String, Int)] mustEqual (JsSuccess("550e8400-e29b-41d4-a716-446655440000" -> 654))
    }

    "JsObject tupled reads new syntax" in {
      implicit val dataReads: Reads[(String, Int)] = (
        (__ \ "uuid").read[String] and
        (__ \ "nb").read[Int]
      ).tupled

      val js = Json.obj(
        "uuid" -> "550e8400-e29b-41d4-a716-446655440000",
        "nb" -> 654
      )

      js.validate[(String, Int)] mustEqual (JsSuccess("550e8400-e29b-41d4-a716-446655440000" -> 654))
    }

    "JsObject tupled writes" in {
      implicit val dataWrites: Writes[(String, Int)] = (
        (__ \ "uuid").write[String] and
        (__ \ "nb").write[Int]
      ).tupled

      val js = Json.obj(
        "uuid" -> "550e8400-e29b-41d4-a716-446655440000",
        "nb" -> 654
      )

      Json.toJson("550e8400-e29b-41d4-a716-446655440000" -> 654) mustEqual (js)
    }

    "JsObject tupled format" in {
      implicit val dataFormat: Format[(String, Int)] = (
        (__ \ "uuid").format[String] and
        (__ \ "nb").format[Int]
      ).tupled

      val js = Json.obj(
        "uuid" -> "550e8400-e29b-41d4-a716-446655440000",
        "nb" -> 654
      )

      Json.toJson("550e8400-e29b-41d4-a716-446655440000" -> 654) mustEqual (js)
      js.validate[(String, Int)] mustEqual (JsSuccess("550e8400-e29b-41d4-a716-446655440000" -> 654))
    }

    "Format simpler syntax without constraints" in {
      val bobby = User("bobby", 54)

      implicit val userFormats = {
        (
          (__ \ 'name).format[String]
          and
          (__ \ 'age).format[Int]
        )(User, unlift(User.unapply))
      }

      val js = Json.toJson(bobby)

      js.validate[User] mustEqual JsSuccess(bobby)
    }

    "Format simpler syntax with constraints" in {
      val bobby = User("bobby", 54)

      implicit val userFormat = (
        (__ \ 'name).format(Reads.minLength[String](5))
        and
        (__ \ 'age).format(Reads.min(40))
      )(User, unlift(User.unapply))

      val js = Json.toJson(bobby)

      js.validate[User] mustEqual JsSuccess(bobby)
    }

    "Compose reads" in {
      val js = Json.obj(
        "field1" -> "alpha",
        "field2" -> 123L,
        "field3" -> Json.obj("field31" -> "beta", "field32" -> 345)
      )
      val reads1 = (__ \ 'field3).json.pick
      val reads2 = ((__ \ 'field32).read[Int] and (__ \ 'field31).read[String]).tupled

      js.validate(reads1 andThen reads2).get mustEqual (345 -> "beta")
    }

    "Apply min/max correctly on ordered types" in {
      val format = Reads.min(1) andKeep Reads.max(3)

      JsNumber(0).validate(format) mustEqual JsError(__, JsonValidationError("error.min", 1))
      JsNumber(1).validate(format) mustEqual JsSuccess(1, __)
      JsNumber(2).validate(format) mustEqual JsSuccess(2, __)
      JsNumber(3).validate(format) mustEqual JsSuccess(3, __)
      JsNumber(4).validate(format) mustEqual JsError(__, JsonValidationError("error.max", 3))
    }
  }

  "JSON generators" should {
    "Build JSON from JSON Reads" in {
      import Reads._
      val js0 = Json.obj(
        "key1" -> "value1",
        "key2" -> Json.obj(
          "key21" -> 123,
          "key23" -> true,
          "key24" -> "blibli"
        ),
        "key3" -> "alpha"
      )

      val js = Json.obj(
        "key1" -> "value1",
        "key2" -> Json.obj(
          "key21" -> 123,
          "key22" -> Json.obj("key222" -> "blabla"),
          "key23" -> true,
          "key24" -> "blibli"
        ),
        "key3" -> Json.arr("alpha", "beta", "gamma")
      )

      val dt = (new java.util.Date).getTime()
      def func = { JsNumber(dt + 100) }

      val jsonTransformer = (
        (__ \ "key1").json.pickBranch and
        (__ \ "key2").json.pickBranch(
          (
            (__ \ "key22").json.update((__ \ "key222").json.pick) and
            (__ \ "key233").json.copyFrom((__ \ "key23").json.pick)
          ).reduce
        ) and
          (__ \ "key3").json.pickBranch[JsArray](pure(Json.arr("delta"))) and
          (__ \ "key4").json.put(
            Json.obj(
              "key41" -> 345,
              "key42" -> "alpha",
              "key43" -> func
            )
          )
      ).reduce

      val res = Json.obj(
        "key1" -> "value1",
        "key2" -> Json.obj(
          "key21" -> 123,
          "key22" -> "blabla",
          "key23" -> true,
          "key24" -> "blibli",
          "key233" -> true
        ),
        "key3" -> Json.arr("delta"),
        "key4" -> Json.obj("key41" -> 345, "key42" -> "alpha", "key43" -> func)
      )

      js0.validate(jsonTransformer) mustEqual (
        //JsError( (__ \ 'key3), "error.expected.jsarray" ) ++
        JsError((__ \ 'key2 \ 'key22), "error.path.missing")
      )

      js.validate(jsonTransformer) mustEqual JsSuccess(res)
    }

  }

  "JSON Reads" should {
    "manage nullable/option" taggedAs (UnstableInScala213) in {
      case class User(name: String, email: String, phone: Option[String])

      implicit val UserReads = (
        (__ \ 'name).read[String] and
        (__ \ 'coords \ 'email).read(Reads.email) and
        (__ \ 'coords \ 'phone).readNullable(Reads.minLength[String](8))
      )(User)

      Json.obj(
        "name" -> "john",
        "coords" -> Json.obj(
          "email" -> "john@xxx.yyy",
          "phone" -> "0123456789"
        )
      ).validate[User] mustEqual (
          JsSuccess(User("john", "john@xxx.yyy", Some("0123456789")))
        )

      Json.obj(
        "name" -> "john",
        "coords" -> Json.obj(
          "email" -> "john@xxx.yyy",
          "phone2" -> "0123456789"
        )
      ).validate[User] mustEqual (
          JsSuccess(User("john", "john@xxx.yyy", None))
        )

      Json.obj(
        "name" -> "john",
        "coords" -> Json.obj(
          "email" -> "john@xxx.yyy"
        )
      ).validate[User] mustEqual (
          JsSuccess(User("john", "john@xxx.yyy", None))
        )

      Json.obj(
        "name" -> "john",
        "coords" -> Json.obj(
          "email" -> "john@xxx.yyy",
          "phone" -> JsNull
        )
      ).validate[User] mustEqual (
          JsSuccess(User("john", "john@xxx.yyy", None))
        )

      Json.obj(
        "name" -> "john",
        "coords2" -> Json.obj(
          "email" -> "john@xxx.yyy",
          "phone" -> "0123456789"
        )
      ).validate[User] mustEqual (
          JsError(Seq(
            __ \ 'coords \ 'phone -> Seq(JsonValidationError("error.path.missing")),
            __ \ 'coords \ 'email -> Seq(JsonValidationError("error.path.missing"))
          ))
        )
    }

    "report correct path for validation errors" in {
      case class User(email: String, phone: Option[String])

      implicit val UserReads = (
        (__ \ 'email).read(Reads.email) and
        (__ \ 'phone).readNullable(Reads.minLength[String](8))
      )(User)

      Json.obj("email" -> "john").validate[User] mustEqual (JsError(__ \ "email", JsonValidationError("error.email")))
      Json.obj("email" -> "john.doe@blibli.com", "phone" -> "4").validate[User] mustEqual (JsError(__ \ "phone", JsonValidationError("error.minLength", 8)))
    }

    "mix reads constraints" in {
      case class User(id: Long, email: String, age: Int)

      implicit val UserReads = (
        (__ \ 'id).read[Long] and
        (__ \ 'email).read(Reads.email andKeep Reads.minLength[String](5)) and
        (__ \ 'age).read(Reads.max(55) or Reads.min(65))
      )(User)

      Json.obj("id" -> 123L, "email" -> "john.doe@blibli.com", "age" -> 50).validate[User] mustEqual (JsSuccess(User(123L, "john.doe@blibli.com", 50)))
      Json.obj("id" -> 123L, "email" -> "john.doe@blibli.com", "age" -> 60).validate[User] mustEqual (JsError((__ \ 'age), JsonValidationError("error.max", 55)) ++ JsError((__ \ 'age), JsonValidationError("error.min", 65)))
      Json.obj("id" -> 123L, "email" -> "john.doe", "age" -> 60).validate[User] mustEqual (JsError((__ \ 'email), JsonValidationError("error.email")) ++ JsError((__ \ 'age), JsonValidationError("error.max", 55)) ++ JsError((__ \ 'age), JsonValidationError("error.min", 65)))
    }

    "recursive reads" in {
      case class User(id: Long, name: String, friend: Option[User] = None)

      implicit lazy val UserReads: Reads[User] = (
        (__ \ 'id).read[Long] and
        (__ \ 'name).read[String] and
        (__ \ 'friend).lazyReadNullable(UserReads)
      )(User)

      val js = Json.obj(
        "id" -> 123L,
        "name" -> "bob",
        "friend" -> Json.obj("id" -> 124L, "name" -> "john", "friend" -> JsNull)
      )

      js.validate[User] mustEqual JsSuccess(User(123L, "bob", Some(User(124L, "john", None))))

      val js2 = Json.obj(
        "id" -> 123L,
        "name" -> "bob",
        "friend" -> Json.obj("id" -> 124L, "name" -> "john")
      )

      js2.validate[User] mustEqual (JsSuccess(User(123L, "bob", Some(User(124L, "john", None)))))
    }

    "recursive writes" in {
      case class User(id: Long, name: String, friend: Option[User] = None)

      implicit lazy val UserWrites: Writes[User] = (
        (__ \ 'id).write[Long] and
        (__ \ 'name).write[String] and
        (__ \ 'friend).lazyWriteNullable(UserWrites)
      )(unlift(User.unapply))

      val js = Json.obj(
        "id" -> 123L,
        "name" -> "bob",
        "friend" -> Json.obj("id" -> 124L, "name" -> "john")
      )

      Json.toJson(User(123L, "bob", Some(User(124L, "john", None)))) mustEqual js
    }

    "recursive formats" in {
      case class User(id: Long, name: String, friend: Option[User] = None)

      implicit lazy val UserFormats: Format[User] = (
        (__ \ 'id).format[Long] and
        (__ \ 'name).format[String] and
        (__ \ 'friend).lazyFormatNullable(UserFormats)
      )(User, unlift(User.unapply))

      val js = Json.obj(
        "id" -> 123L,
        "name" -> "bob",
        "friend" -> Json.obj("id" -> 124L, "name" -> "john")
      )

      js.validate[User] mustEqual JsSuccess(User(123L, "bob", Some(User(124L, "john", None))))
      Json.toJson(User(123L, "bob", Some(User(124L, "john", None)))) mustEqual js
    }

    "lots of fields to read" in {
      val myReads = (
        (__ \ 'field1).read[String] and
        (__ \ 'field2).read[Long] and
        (__ \ 'field3).read[Float] and
        (__ \ 'field4).read[Boolean] and
        (__ \ 'field5).read[List[String]] and
        (__ \ 'field6).read[String] and
        (__ \ 'field7).read[String] and
        (__ \ 'field8).read[String] and
        (__ \ 'field9).read[String] and
        (__ \ 'field10).read[String] and
        (__ \ 'field11).read[String] and
        (__ \ 'field12).read[String]
      ).tupled

      Json.obj(
        "field1" -> "val1",
        "field2" -> 123L,
        "field3" -> 123.456F,
        "field4" -> true,
        "field5" -> Json.arr("alpha", "beta"),
        "field6" -> "val6",
        "field7" -> "val7",
        "field8" -> "val8",
        "field9" -> "val9",
        "field10" -> "val10",
        "field11" -> "val11",
        "field12" -> "val12"
      ).validate(myReads) mustEqual (
          JsSuccess(("val1", 123L, 123.456F, true, List("alpha", "beta"), "val6", "val7", "val8", "val9", "val10", "val11", "val12"))
        )
    }

    "single field case class" in {
      case class Test(field: String)
      val myFormat = (__ \ 'field).format[String].inmap(Test, unlift(Test.unapply))

      myFormat.reads(Json.obj("field" -> "blabla")) mustEqual JsSuccess(Test("blabla"), __ \ 'field)
      myFormat.reads(Json.obj()) mustEqual JsError(__ \ 'field, "error.path.missing")
      myFormat.writes(Test("blabla")) mustEqual Json.obj("field" -> "blabla")
    }

    "reduce Reads[JsObject]" in {
      import Reads._

      val myReads: Reads[JsObject] = (
        (__ \ 'field1).json.pickBranch and
        (__ \ 'field2).json.pickBranch
      ).reduce

      val js0 = Json.obj("field1" -> "alpha")
      val js = js0 ++ Json.obj("field2" -> Json.obj("field21" -> 123, "field22" -> true))
      val js2 = js ++ Json.obj("field3" -> "beta")
      js.validate(myReads) mustEqual JsSuccess(js)
      js2.validate(myReads) mustEqual JsSuccess(js)
      js0.validate(myReads) mustEqual JsError(__ \ 'field2, "error.path.missing")
    }

    "reduce Reads[JsArray]" in {
      import Reads._

      val myReads: Reads[JsArray] = (
        (__ \ 'field1).json.pick[JsString] and
        (__ \ 'field2).json.pick[JsNumber] and
        (__ \ 'field3).json.pick[JsBoolean]
      ).reduce[JsValue, JsArray]

      val js0 = Json.obj("field1" -> "alpha")
      val js = js0 ++ Json.obj("field2" -> 123L, "field3" -> false)
      val js2 = js ++ Json.obj("field4" -> false)
      js.validate(myReads) mustEqual (JsSuccess(Json.arr("alpha", 123L, false)))
      js2.validate(myReads) mustEqual (JsSuccess(Json.arr("alpha", 123L, false)))
      js0.validate(myReads) mustEqual (JsError(__ \ 'field2, "error.path.missing") ++ JsError(__ \ 'field3, "error.path.missing"))
    }

    "reduce Reads[JsArray] no type" in {
      import Reads._

      val myReads: Reads[JsArray] = (
        (__ \ 'field1).json.pick and
        (__ \ 'field2).json.pick and
        (__ \ 'field3).json.pick
      ).reduce

      val js0 = Json.obj("field1" -> "alpha")
      val js = js0 ++ Json.obj("field2" -> 123L, "field3" -> false)
      val js2 = js ++ Json.obj("field4" -> false)
      js.validate(myReads) mustEqual JsSuccess(Json.arr("alpha", 123L, false))
      js2.validate(myReads) mustEqual JsSuccess(Json.arr("alpha", 123L, false))
      js0.validate(myReads) mustEqual JsError(__ \ 'field2, "error.path.missing") ++ JsError(__ \ 'field3, "error.path.missing")
    }

    "serialize JsError to json" in {
      val jserr = JsError(Seq(
        (__ \ 'field1 \ 'field11) -> Seq(
          JsonValidationError(Seq("msg1.msg11", "msg1.msg12"), "arg11", 123L, 123.456F),
          JsonValidationError("msg2.msg21.msg22", 456, 123.456, true, 123)
        ),
        (__ \ 'field2 \ 'field21) -> Seq(
          JsonValidationError("msg1.msg21", "arg1", Json.obj("test" -> "test2")),
          JsonValidationError("msg2", "arg1", "arg2")
        )
      ))

      val json = Json.obj(
        "obj.field1.field11" -> Json.arr(
          Json.obj(
            "msg" -> Json.arr("msg1.msg11", "msg1.msg12"),
            "args" -> Json.arr("arg11", 123, 123.456F)
          ),
          Json.obj(
            "msg" -> Json.arr("msg2.msg21.msg22"),
            "args" -> Json.arr(456, 123.456, true, 123)
          )
        ),
        "obj.field2.field21" -> Json.arr(
          Json.obj(
            "msg" -> Json.arr("msg1.msg21"),
            "args" -> Json.arr("arg1", Json.obj("test" -> "test2"))
          ),
          Json.obj(
            "msg" -> Json.arr("msg2"),
            "args" -> Json.arr("arg1", "arg2")
          )
        )
      )

      JsError.toJson(jserr) mustEqual json
    }

    "prune json" in {
      import Reads._

      val js = Json.obj(
        "field1" -> "alpha",
        "field2" -> Json.obj("field21" -> 123, "field22" -> true, "field23" -> "blabla"),
        "field3" -> "beta"
      )

      val res = Json.obj(
        "field1" -> "alpha",
        "field2" -> Json.obj("field22" -> true),
        "field3" -> "beta"
      )

      val myReads: Reads[JsObject] = (
        (__ \ 'field1).json.pickBranch and
        (__ \ 'field2).json.pickBranch(
          (__ \ 'field21).json.prune andThen (__ \ 'field23).json.prune
        ) and
          (__ \ 'field3).json.pickBranch
      ).reduce

      js.validate(myReads) mustEqual JsSuccess(res)
    }
  }

  "JSON Writes" should {
    "manage option" in {
      import Writes._

      case class User(email: String, phone: Option[String])

      implicit val UserWrites = (
        (__ \ 'email).write[String] and
        (__ \ 'phone).writeNullable[String]
      )(unlift(User.unapply))

      Json.toJson(User("john.doe@blibli.com", None)) mustEqual Json.obj("email" -> "john.doe@blibli.com")
      Json.toJson(User("john.doe@blibli.com", Some("12345678"))) mustEqual Json.obj("email" -> "john.doe@blibli.com", "phone" -> "12345678")
    }

    "join" in {
      val joinWrites = (
        (__ \ 'alpha).write[JsString] and
        (__ \ 'beta).write[JsValue]
      ).join

      joinWrites.writes(JsString("toto")) mustEqual Json.obj("alpha" -> "toto", "beta" -> "toto")

      val joinWrites2 = (
        (__ \ 'alpha).write[JsString] and
        (__ \ 'beta).write[JsValue] and
        (__ \ 'gamma).write[JsString] and
        (__ \ 'delta).write[JsValue]
      ).join

      joinWrites2.writes(JsString("toto")) mustEqual (
        Json.obj("alpha" -> "toto", "beta" -> "toto", "gamma" -> "toto", "delta" -> "toto")
      )
    }
  }

  "JSON Format" should {
    "manage option" in {
      import Reads._
      import Writes._

      case class User(email: String, phone: Option[String])

      implicit val UserFormat = (
        (__ \ 'email).format(email) and
        (__ \ 'phone).formatNullable(Format(minLength[String](8), Writes.of[String]))
      )(User, unlift(User.unapply))

      Json.obj("email" -> "john").validate[User] mustEqual JsError(__ \ "email", JsonValidationError("error.email"))
      Json.obj("email" -> "john.doe@blibli.com", "phone" -> "4").validate[User] mustEqual (JsError(__ \ "phone", JsonValidationError("error.minLength", 8)))
      Json.obj("email" -> "john.doe@blibli.com", "phone" -> "12345678").validate[User] mustEqual (JsSuccess(User("john.doe@blibli.com", Some("12345678"))))
      Json.obj("email" -> "john.doe@blibli.com").validate[User] mustEqual (JsSuccess(User("john.doe@blibli.com", None)))

      Json.toJson(User("john.doe@blibli.com", None)) mustEqual Json.obj("email" -> "john.doe@blibli.com")
      Json.toJson(User("john.doe@blibli.com", Some("12345678"))) mustEqual Json.obj("email" -> "john.doe@blibli.com", "phone" -> "12345678")
    }
  }

  "JsResult" should {
    "be usable in for-comprehensions" in {
      val res = JsSuccess("foo")
      def x = for {
        s <- res
        if s.size < 5
      } yield 42

      x mustEqual JsSuccess(42)
    }

    "be a functor" when {
      "JsSuccess" in {
        val res1: JsResult[String] = JsSuccess("foo", JsPath(List(KeyPathNode("bar"))))
        res1.map(identity) mustEqual res1
      }

      "JsError" in {
        val res2: JsResult[String] = JsError(Seq(JsPath(List(KeyPathNode("bar"))) -> Seq(JsonValidationError("baz.bah"))))
        res2.map(identity) mustEqual res2
      }
    }

    "have filtering methods that allow users to customize the error" in {
      val res: JsResult[String] = JsSuccess("foo")
      val error = JsError(__ \ "bar", "There is a problem")

      res.filter(error)(_ != "foo") mustEqual error
      res.filter(error)(_ == "foo") mustEqual res
      res.filterNot(error)(_ == "foo") mustEqual error
      res.filterNot(error)(_ != "foo") mustEqual res
    }
  }
}
