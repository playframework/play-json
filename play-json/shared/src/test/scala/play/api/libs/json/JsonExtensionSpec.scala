/*
 * Copyright (C) 2009-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import play.api.libs.json.JsonNaming.SnakeCase

import play.api.libs.json.Json._

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

case class User(age: Int, name: String)
case class Dog(name: String, master: User)
case class UserProfile(firstName: String, lastName: String, zip: Option[String], _homeCity: String)
object UserProfile {
  def obj1  = UserProfile("Christian", "Schmitt", None, "Kenzingen")
  def json1 = Json.obj("first_name" -> "Christian", "last_name" -> "Schmitt", "home_city" -> "Kenzingen")
  def json2 =
    Json.obj(
      "lightbend_firstName" -> "Christian",
      "lightbend_lastName"  -> "Schmitt",
      "lightbend__homeCity" -> "Kenzingen"
    )
  def json3 = Json.obj("FirstName" -> "Christian", "LastName" -> "Schmitt", "_homeCity" -> "Kenzingen")
}
case class UserProfileHolder(holder: String, profile: UserProfile)
case class Cat(name: String)

case class RecUser(
    name: String,
    cat: Option[Cat] = None,
    hobbies: List[String] = List(),
    friends: List[RecUser] = List()
)

case class User1(name: String, friend: Option[User1] = None)

case class UserMap(name: String, friends: Map[String, UserMap] = Map())

case class Toto(name: String)
case class Toto2(name: Option[String])
case class Toto3(name: List[Double])
case class Toto4(name: Set[Long])
case class Toto5(name: Map[String, Int])
case class Toto6(name: Seq[Dog])
case class UserFail(name: String, bd: Toto)

case class Id[A](id: A)
case class C1[A](id: Id[A], name: String)

case class X(
    _1: String,
    _2: String,
    _3: String,
    _4: String,
    _5: String,
    _6: String,
    _7: String,
    _8: String,
    _9: String,
    _10: String,
    _11: String,
    _12: String,
    _13: String,
    _14: String,
    _15: String,
    _16: String,
    _17: String,
    _18: String,
    _19: String,
    _20: String,
    _21: String,
    _22: String
)

case class Program(id: Long, name: String, logoPath: Option[String], logoThumb: Option[String])
object Program {
  def programs = List.empty[Program]
}

case class Person(name: String, age: Int)
object Person {
  implicit val personReads: Reads[Person]    = Json.reads[Person]
  implicit val personWrites: OWrites[Person] = Json.writes[Person]
}

package foreign {
  case class Foreigner(name: String)
}
object ForeignTest {
  implicit val foreignerReads: Reads[foreign.Foreigner]    = Json.reads[foreign.Foreigner]
  implicit val foreignerWrites: OWrites[foreign.Foreigner] = Json.writes[foreign.Foreigner]
}

case class Person2(names: List[String])

case class GenericCaseClass[A](obj: A)
case class GenericCaseClass2[A, B](obj1: A, obj2: B)
case class WrappedGenericInt(int: GenericCaseClass[Int])
case class WrappedGenericIntString(intString: GenericCaseClass2[Int, String])

case class VarArgsOnly(ints: Int*)
case class LastVarArg(name: String, ints: Int*)

object Person2 {
  implicit val person2Fmt: OFormat[Person2] = Json.format[Person2]
}

case class CustomApply(a: Int, b: String)
object CustomApply {
  def apply(): CustomApply = apply(10, "foo")
}

case class Optional(props: Option[String])

class JsonExtensionSpec extends AnyWordSpec with Matchers {
  "JsonExtension" should {
    "create a reads[User]" in {
      import play.api.libs.json.Json

      // object User {def apply(age:Int):User = User(age,"")}
      implicit val userReads: Reads[User] = Json.reads[User]

      Json.fromJson[User](Json.obj("name" -> "toto", "age" -> 45)).mustEqual(JsSuccess(User(45, "toto")))
    }

    "create a writes[User]" in {
      import play.api.libs.json.Json

      implicit val userWrites: OWrites[User] = Json.writes[User]

      Json.toJson(User(45, "toto")).mustEqual(Json.obj("name" -> "toto", "age" -> 45))
    }

    "create a format[User]" in {
      import play.api.libs.json.Json

      implicit val userFormat: OFormat[User] = Json.format[User]

      Json.fromJson[User](Json.obj("name" -> "toto", "age" -> 45)).mustEqual(JsSuccess(User(45, "toto")))

      Json.toJson(User(45, "toto")).mustEqual(Json.obj("name" -> "toto", "age" -> 45))
    }

    "create a reads[Dog]" in {
      import play.api.libs.json.Json

      implicit val userReads: Reads[User] = Json.reads[User]
      implicit val dogReads: Reads[Dog]   = Json.reads[Dog]

      Json
        .fromJson[Dog](
          Json.obj(
            "name"   -> "medor",
            "master" -> Json.obj("name" -> "toto", "age" -> 45)
          )
        )
        .mustEqual(JsSuccess(Dog("medor", User(45, "toto"))))
    }

    "create a writes[Dog]" in {
      import play.api.libs.json.Json

      implicit val userWrites: OWrites[User] = Json.writes[User]
      implicit val dogWrites: OWrites[Dog]   = Json.writes[Dog]

      Json
        .toJson(Dog("medor", User(45, "toto")))
        .mustEqual(
          Json.obj(
            "name"   -> "medor",
            "master" -> Json.obj("name" -> "toto", "age" -> 45)
          )
        )
    }

    "create a format[Dog]" in {
      import play.api.libs.json.Json

      implicit val userFormat: OFormat[User] = Json.format[User]
      implicit val dogFormat: OFormat[Dog]   = Json.format[Dog]

      Json
        .fromJson[Dog](
          Json.obj(
            "name"   -> "medor",
            "master" -> Json.obj("name" -> "toto", "age" -> 45)
          )
        )
        .mustEqual(JsSuccess(Dog("medor", User(45, "toto"))))

      Json
        .toJson(Dog("medor", User(45, "toto")))
        .mustEqual(
          Json.obj(
            "name"   -> "medor",
            "master" -> Json.obj("name" -> "toto", "age" -> 45)
          )
        )
    }

    "create a reads[RecUser]" in {
      import play.api.libs.json.Json

      implicit val catReads: Reads[Cat]         = Json.reads[Cat]
      implicit val recUserReads: Reads[RecUser] = Json.reads[RecUser]

      Json
        .fromJson[RecUser](
          Json.obj(
            "name"    -> "bob",
            "cat"     -> Json.obj("name" -> "minou"),
            "hobbies" -> Json.arr("bobsleig", "manhunting"),
            "friends" -> Json.arr(Json.obj("name" -> "tom", "hobbies" -> Json.arr(), "friends" -> Json.arr()))
          )
        )
        .mustEqual(
          JsSuccess(
            RecUser(
              "bob",
              Some(Cat("minou")),
              List("bobsleig", "manhunting"),
              List(RecUser("tom"))
            )
          )
        )
    }

    "create a writes[RecUser]" in {
      import play.api.libs.json.Json

      implicit val catWrites: OWrites[Cat]         = Json.writes[Cat]
      implicit val recUserWrites: OWrites[RecUser] = Json.writes[RecUser]

      Json
        .toJson(
          RecUser(
            "bob",
            Some(Cat("minou")),
            List("bobsleig", "manhunting"),
            List(RecUser("tom"))
          )
        )
        .mustEqual(
          Json.obj(
            "name"    -> "bob",
            "cat"     -> Json.obj("name" -> "minou"),
            "hobbies" -> Json.arr("bobsleig", "manhunting"),
            "friends" -> Json.arr(Json.obj("name" -> "tom", "hobbies" -> Json.arr(), "friends" -> Json.arr()))
          )
        )
    }

    "create a format[RecUser]" in {
      import play.api.libs.json.Json

      implicit val catFormat: OFormat[Cat]         = Json.format[Cat]
      implicit val recUserFormat: OFormat[RecUser] = Json.format[RecUser]

      Json
        .fromJson[RecUser](
          Json.obj(
            "name"    -> "bob",
            "cat"     -> Json.obj("name" -> "minou"),
            "hobbies" -> Json.arr("bobsleig", "manhunting"),
            "friends" -> Json.arr(Json.obj("name" -> "tom", "hobbies" -> Json.arr(), "friends" -> Json.arr()))
          )
        )
        .mustEqual(
          JsSuccess(
            RecUser(
              "bob",
              Some(Cat("minou")),
              List("bobsleig", "manhunting"),
              List(RecUser("tom"))
            )
          )
        )

      Json
        .toJson(
          RecUser(
            "bob",
            Some(Cat("minou")),
            List("bobsleig", "manhunting"),
            List(RecUser("tom"))
          )
        )
        .mustEqual(
          Json.obj(
            "name"    -> "bob",
            "cat"     -> Json.obj("name" -> "minou"),
            "hobbies" -> Json.arr("bobsleig", "manhunting"),
            "friends" -> Json.arr(Json.obj("name" -> "tom", "hobbies" -> Json.arr(), "friends" -> Json.arr()))
          )
        )
    }

    "create a reads[User1]" in {
      import play.api.libs.json.Json

      implicit val userReads: Reads[User1] = Json.reads[User1]

      Json
        .fromJson[User1](
          Json.obj(
            "name"   -> "bob",
            "friend" -> Json.obj("name" -> "tom")
          )
        )
        .mustEqual(
          JsSuccess(
            User1(
              "bob",
              Some(User1("tom"))
            )
          )
        )
    }

    "create a writes[User1]" in {
      import play.api.libs.json.Json

      implicit val userWrites: OWrites[User1] = Json.writes[User1]

      Json
        .toJson(
          User1(
            "bob",
            Some(User1("tom"))
          )
        )
        .mustEqual(
          Json.obj(
            "name"   -> "bob",
            "friend" -> Json.obj("name" -> "tom")
          )
        )
    }

    "create a format[User1]" in {
      import play.api.libs.json.Json

      implicit val userFormat: OFormat[User1] = Json.format[User1]

      Json
        .fromJson[User1](
          Json.obj(
            "name"   -> "bob",
            "friend" -> Json.obj("name" -> "tom")
          )
        )
        .mustEqual(
          JsSuccess(
            User1(
              "bob",
              Some(User1("tom"))
            )
          )
        )

      Json
        .toJson(
          User1(
            "bob",
            Some(User1("tom"))
          )
        )
        .mustEqual(
          Json.obj(
            "name"   -> "bob",
            "friend" -> Json.obj("name" -> "tom")
          )
        )
    }

    "create a format[WrappedGenericInt]" in {
      import play.api.libs.json.Json._
      import play.api.libs.functional.syntax._

      implicit def genericFormat[A: Format]: Format[GenericCaseClass[A]] =
        (
          (
            (__ \ "obj").format[A]
          ).inmap
        )(GenericCaseClass[A] _, x => (x.obj))

      implicit val wrappedGenericIntFormat: OFormat[WrappedGenericInt] = Json.format[WrappedGenericInt]

      val genericInt = GenericCaseClass(obj = 1)
      val wrapped    = WrappedGenericInt(int = genericInt)

      val expectedJsObj = Json.obj(
        "int" -> Json.obj("obj" -> 1)
      )

      Json.toJson(wrapped).mustEqual(expectedJsObj)
      Json.fromJson[WrappedGenericInt](expectedJsObj).mustEqual(JsSuccess(wrapped))
    }

    "create a format[WrappedGenericIntString]" in {
      import play.api.libs.json.Json._
      import play.api.libs.functional.syntax._

      implicit def genericEntityWrapperFormat[A: Format, B: Format]: Format[GenericCaseClass2[A, B]] =
        (
          (__ \ "obj1").format[A] and
            (__ \ "obj2").format[B]
        )(GenericCaseClass2[A, B] _, x => (x.obj1, x.obj2))

      implicit val genericHolderFormat: OFormat[WrappedGenericIntString] = Json.format[WrappedGenericIntString]

      val genericIntString = GenericCaseClass2(obj1 = 1, obj2 = "hello")
      val genericHolder    = WrappedGenericIntString(intString = genericIntString)
      val expectedJsObj = Json.obj(
        "intString" -> Json.obj("obj1" -> 1, "obj2" -> "hello")
      )
      Json.toJson(genericHolder).mustEqual(expectedJsObj)
      Json.fromJson[WrappedGenericIntString](expectedJsObj).mustEqual(JsSuccess(genericHolder))
    }

    "VarArgsOnly reads, writes, format" should {
      val reads: Reads[VarArgsOnly]    = Json.reads[VarArgsOnly]
      val writes: OWrites[VarArgsOnly] = Json.writes[VarArgsOnly]
      val format: OFormat[VarArgsOnly] = Json.format[VarArgsOnly]

      val obj   = VarArgsOnly(1, 2, 3)
      val jsObj = Json.obj("ints" -> Seq(1, 2, 3))

      "formats should be able to read and write" in {
        Json.toJson(obj)(format).mustEqual(jsObj)
        jsObj.as[VarArgsOnly](format).mustEqual(obj)
      }

      "reads should be able to read valid Json and ignore invalid Json" in {
        jsObj.as[VarArgsOnly](reads).mustEqual(obj)
        Json.fromJson[VarArgsOnly](Json.obj("hello" -> "world"))(reads).isError.mustEqual(true)
      }

      "writes should be able to spit out valid json" in {
        Json.toJson(obj)(writes).mustEqual(jsObj)
      }
    }

    "LastVarArg reads, writes, format" should {
      val reads: Reads[LastVarArg]    = Json.reads[LastVarArg]
      val writes: OWrites[LastVarArg] = Json.writes[LastVarArg]
      val format: OFormat[LastVarArg] = Json.format[LastVarArg]

      val obj   = LastVarArg("hello", 1, 2, 3)
      val jsObj = Json.obj("name" -> "hello", "ints" -> Seq(1, 2, 3))

      "formats should be able to read and write" in {
        Json.toJson(obj)(format).mustEqual(jsObj)
        jsObj.as[LastVarArg](format).mustEqual(obj)
      }

      "reads should be able to read valid Json and ignore invalid Json" in {
        jsObj.as[LastVarArg](reads).mustEqual(obj)
        Json.fromJson[LastVarArg](Json.obj("hello" -> "world"))(reads).isError.mustEqual(true)
      }

      "writes should be able to spit out valid json" in {
        Json.toJson(obj)(writes).mustEqual(jsObj)
      }
    }

    "manage Map[String, User]" in {
      import play.api.libs.json.Json

      implicit val userReads: Reads[UserMap] = Json.reads[UserMap]

      Json
        .fromJson[UserMap](
          Json
            .obj("name" -> "toto", "friends" -> Json.obj("tutu" -> Json.obj("name" -> "tutu", "friends" -> Json.obj())))
        )
        .mustEqual(
          JsSuccess(UserMap("toto", Map("tutu" -> UserMap("tutu"))))
        )
    }

    "manage Boxed class" in {
      import play.api.libs.functional.syntax._

      implicit def idReads[A](implicit rds: Reads[A]): Reads[Id[A]] =
        Reads[Id[A]] { js =>
          rds.reads(js).map(Id[A](_))
        }

      // val c2Reads1 = Json.reads[C2]

      implicit def c1Reads[A](implicit rds: Reads[Id[A]]): Reads[C1[A]] = {
        (
          (__ \ Symbol("id")).read(rds) and
            (__ \ Symbol("name")).read[String]
        )((id, name) => C1[A](id, name))
      }

      val js = Json.obj("id" -> 123L, "name" -> "toto")

      js.validate(c1Reads[Long]).mustEqual(JsSuccess(C1[Long](Id[Long](123L), "toto")))
    }

    /**
     * test to validate it doesn't compile if missing implicit
     * "fail if missing " in {
     * import play.api.libs.json.Json
     *
     * implicit val userReads = Json.reads[UserFail]
     *
     * success
     * }
     */
    "test 21 fields" in {
      Json.reads[X]
      Json.writes[X]
      Json.format[X]
      ()
    }

    "test inception with overridden object" in {
      Json.reads[Program]
      ()
    }

    "test case class 1 field" in {
      Json.reads[Toto]
      Json.writes[Toto]
      Json.format[Toto]
      ()
    }

    "test case class 1 field option" in {
      Json.reads[Toto2]
      Json.writes[Toto2]
      Json.format[Toto2]
      ()
    }

    "test case class 1 field list" in {
      Json.reads[Toto3]
      Json.writes[Toto3]
      Json.format[Toto3]
      ()
    }

    "test case class 1 field set" in {
      Json.reads[Toto4]
      Json.writes[Toto4]
      Json.format[Toto4]
      ()
    }

    "test case class 1 field map" in {
      Json.reads[Toto5]
      Json.writes[Toto5]
      Json.format[Toto5]
      ()
    }

    "test case class 1 field seq[Dog]" in {
      implicit val userFormat: OFormat[User] = Json.format[User]
      implicit val dogFormat: OFormat[Dog]   = Json.format[Dog]
      Json.reads[Toto6]
      Json.writes[Toto6]
      implicit val toto6Format: OFormat[Toto6] = Json.format[Toto6]

      val js = Json.obj(
        "name" -> Json.arr(
          Json.obj(
            "name"   -> "medor",
            "master" -> Json.obj("name" -> "toto", "age" -> 45)
          ),
          Json.obj(
            "name"   -> "brutus",
            "master" -> Json.obj("name" -> "tata", "age" -> 23)
          )
        )
      )

      Json
        .fromJson[Toto6](js)
        .get
        .mustEqual(
          Toto6(
            Seq(
              Dog("medor", User(45, "toto")),
              Dog("brutus", User(23, "tata"))
            )
          )
        )
    }

    "test case reads in companion object" in {
      Json.fromJson[Person](Json.toJson(Person("bob", 15))).get.mustEqual(Person("bob", 15))
    }

    "test case single-field in companion object" in {
      Json.fromJson[Person2](Json.toJson(Person2(List("bob", "bobby")))).get.mustEqual(Person2(List("bob", "bobby")))
    }

    "test hygiene" in {
      val play = ""
      type LazyHelper = Any; val LazyHelper = ()
      val scala = ""
      type String = Any; val String  = ""
      type Unit   = Any; val Unit    = ""
      type Any    = Nothing; val Any = ""
      type Int    = String; val Int  = ""

      Json.reads[Toto2]
      Json.writes[Toto2]
      Json.format[Toto2]
      ()
    }

    "create a format[CustomApply]" in {
      implicit val fmt: OFormat[CustomApply] = Json.format[CustomApply]

      Json.fromJson[CustomApply](Json.obj("a" -> 5, "b" -> "foo")).mustEqual(JsSuccess(CustomApply(5, "foo")))
      Json.toJson(CustomApply(5, "foo")).mustEqual(Json.obj("a" -> 5, "b" -> "foo"))
      Json.toJson(CustomApply()).mustEqual(Json.obj("a" -> 10, "b" -> "foo"))
    }

    "create a writes[UserProfile] with SnakeCase" in {
      import play.api.libs.json.Json

      implicit val jsonConfiguration: JsonConfiguration = JsonConfiguration(naming = JsonNaming.SnakeCase)
      implicit val writes: OWrites[UserProfile]         = Json.writes[UserProfile]

      Json.toJson(UserProfile.obj1).mustEqual(UserProfile.json1)
    }

    "create a reads[UserProfile] with SnakeCase" in {
      import play.api.libs.json.Json

      implicit val jsonConfiguration: JsonConfiguration = JsonConfiguration(naming = JsonNaming.SnakeCase)
      implicit val reads: Reads[UserProfile]            = Json.reads[UserProfile]

      Json.fromJson(UserProfile.json1).mustEqual(JsSuccess(UserProfile.obj1))
    }

    "create a format[UserProfile] with SnakeCase" in {
      import play.api.libs.json.Json

      implicit val jsonConfiguration: JsonConfiguration = JsonConfiguration(naming = JsonNaming.SnakeCase)
      implicit val format: OFormat[UserProfile]         = Json.format[UserProfile]

      Json.fromJson(UserProfile.json1).mustEqual(JsSuccess(UserProfile.obj1))
      Json.toJson(UserProfile.obj1).mustEqual(UserProfile.json1)
    }

    "create a writes[UserProfile] with PascalCase" in {
      import play.api.libs.json.Json

      implicit val jsonConfiguration: JsonConfiguration = JsonConfiguration(naming = JsonNaming.PascalCase)
      implicit val writes: OWrites[UserProfile]         = Json.writes[UserProfile]

      Json.toJson(UserProfile.obj1).mustEqual(UserProfile.json3)
    }

    "create a reads[UserProfile] with PascalCase" in {
      import play.api.libs.json.Json

      implicit val jsonConfiguration: JsonConfiguration = JsonConfiguration(naming = JsonNaming.PascalCase)
      implicit val reads: Reads[UserProfile]            = Json.reads[UserProfile]

      Json.fromJson(UserProfile.json3).mustEqual(JsSuccess(UserProfile.obj1))
    }

    "create a format[UserProfile] with PascalCase" in {
      import play.api.libs.json.Json

      implicit val jsonConfiguration: JsonConfiguration = JsonConfiguration(naming = JsonNaming.PascalCase)
      implicit val format: OFormat[UserProfile]         = Json.format[UserProfile]

      Json.fromJson(UserProfile.json3).mustEqual(JsSuccess(UserProfile.obj1))
      Json.toJson(UserProfile.obj1).mustEqual(UserProfile.json3)
    }

    "create a writes[UserProfile] with CustomNaming" in {
      import play.api.libs.json.Json

      object LightbendJsonNaming extends JsonNaming {
        override def apply(property: String): String = s"lightbend_$property"
      }

      implicit val jsonConfiguration: JsonConfiguration = JsonConfiguration(LightbendJsonNaming)
      implicit val writes: OWrites[UserProfile]         = Json.writes[UserProfile]

      Json.toJson(UserProfile.obj1).mustEqual(UserProfile.json2)
    }

    "create a reads[UserProfile] with CustomNaming" in {
      import play.api.libs.json.Json

      object LightbendJsonNaming extends JsonNaming {
        override def apply(property: String): String = s"lightbend_$property"
      }

      implicit val jsonConfiguration: JsonConfiguration = JsonConfiguration(LightbendJsonNaming)
      implicit val reads: Reads[UserProfile]            = Json.reads[UserProfile]

      Json.fromJson(UserProfile.json2).mustEqual(JsSuccess(UserProfile.obj1))
    }

    "create a format[UserProfile] with CustomNaming" in {
      import play.api.libs.json.Json

      object LightbendJsonNaming extends JsonNaming {
        override def apply(property: String): String = s"lightbend_$property"
      }

      implicit val jsonConfiguration: JsonConfiguration = JsonConfiguration(LightbendJsonNaming)
      implicit val format: OFormat[UserProfile]         = Json.format[UserProfile]

      Json.fromJson(UserProfile.json2).mustEqual(JsSuccess(UserProfile.obj1))
      Json.toJson(UserProfile.obj1).mustEqual(UserProfile.json2)
    }

    "create a stacked format[UserProfile] with SnakeCase" in {
      import play.api.libs.json.Json

      implicit val jsonConfiguration: JsonConfiguration = JsonConfiguration(SnakeCase)
      implicit val format1: OFormat[UserProfile]        = Json.format[UserProfile]
      implicit val format2: OFormat[UserProfileHolder]  = Json.format[UserProfileHolder]

      Json
        .fromJson[UserProfileHolder](Json.obj("holder" -> "Christian", "profile" -> UserProfile.json1))
        .mustEqual(JsSuccess(UserProfileHolder("Christian", UserProfile.obj1)))
      Json
        .toJson(UserProfileHolder("Christian", UserProfile.obj1))
        .mustEqual(Json.obj("holder" -> "Christian", "profile" -> UserProfile.json1))
    }

    "create a Writes[Optional] with optionHandlers=WritesNull" in {
      implicit val jsonConfiguration: JsonConfiguration = JsonConfiguration(optionHandlers = OptionHandlers.WritesNull)
      val writer                                        = Json.writes[Optional]
      writer.writes(Optional(None)).mustEqual(Json.obj("props" -> JsNull))
    }

    "create a Format[Optional] with optionHandlers=WritesNull" in {
      implicit val jsonConfiguration: JsonConfiguration = JsonConfiguration(optionHandlers = OptionHandlers.WritesNull)
      val formatter                                     = Json.format[Optional]
      formatter.writes(Optional(None)).mustEqual(Json.obj("props" -> JsNull))

      formatter.reads(Json.obj()).mustEqual(JsSuccess(Optional(None)))
      formatter.reads(Json.obj("props" -> JsNull)).mustEqual(JsSuccess(Optional(None)))
      formatter.reads(Json.obj("props" -> Some("foo"))).mustEqual(JsSuccess(Optional(Some("foo"))))
    }
  }
}
