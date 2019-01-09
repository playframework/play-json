/*
 * Copyright (C) 2009-2019 Lightbend Inc. <https://www.lightbend.com>
 */

package scalaguide.json

import play.api.libs.json.Json
import org.specs2.mutable.Specification
import play.api.libs.json.JsonNaming.SnakeCase

//#valueClass
final class IdText(val value: String) extends AnyVal
//#valueClass

class ScalaJsonAutomatedSpec extends Specification {
  //#model
  case class Resident(name: String, age: Int, role: Option[String])
  //#model

  //#model2
  case class PlayUser(name: String, firstName: String, userAge: Int)
  //#model2

  //#model3
  sealed trait Role
  case object Admin extends Role
  class Contributor(val organization: String) extends Role {
    override def equals(obj: Any): Boolean = obj match {
      case other: Contributor if obj != null ⇒ this.organization == other.organization
      case _ ⇒ false
    }
  }
  object Contributor {
    def apply(organization: String): Contributor = new Contributor(organization)
    def unapply(contributor: Contributor): Option[(String)] = Some(contributor.organization)
  }
  //#model3

  val sampleJson = Json.parse(
    """{
      "name" : "Fiver",
      "age" : 4
    }"""
  )
  val sampleData = Resident("Fiver", 4, None)

  val sampleJson2 = Json.parse(
    """{
      "name": "Schmitt",
      "first_name": "Christian",
      "user_age": 26
    }"""
  )
  val sampleJson3 = Json.parse(
    """{
      "lightbend_name": "Schmitt",
      "lightbend_firstName": "Christian",
      "lightbend_userAge": 26
    }"""
  )
  val sampleData2 = PlayUser("Schmitt", "Christian", 26)

  val sampleJson4 = Json.parse(
    """{
      "name": "Fiver",
      "age": 4,
      "role": null
    }"""
  )

  "Scala JSON automated" should {
    "for case class" >> {
      "produce a Reads" in {
        //#auto-reads
        import play.api.libs.json._

        implicit val residentReads = Json.reads[Resident]
        //#auto-reads

        sampleJson.as[Resident] must_=== sampleData
      }

      "do the same thing as a manual Reads" in {
        //#manual-reads
        import play.api.libs.json._
        import play.api.libs.functional.syntax._

        implicit val residentReads = (
          (__ \ "name").read[String] and
          (__ \ "age").read[Int] and
          (__ \ "role").readNullable[String]
        )(Resident)
        //#manual-reads

        sampleJson.as[Resident] must_=== sampleData
      }

      "produce a Writes" in {
        //#auto-writes
        import play.api.libs.json._

        implicit val residentWrites = Json.writes[Resident]
        //#auto-writes

        Json.toJson(sampleData) must_=== sampleJson
      }

      "produce a Format" in {
        //#auto-format
        import play.api.libs.json._

        implicit val residentFormat = Json.format[Resident]
        //#auto-format

        sampleJson.as[Resident] must_=== sampleData and {
          Json.toJson(sampleData) must_=== sampleJson
        }
      }

      "produce a Writes with SnakeCase" in {
        //#auto-naming-writes
        import play.api.libs.json._

        implicit val config = JsonConfiguration(SnakeCase)

        implicit val userWrites: OWrites[PlayUser] = Json.writes[PlayUser]
        //#auto-naming-writes

        Json.toJson(sampleData2) must_=== sampleJson2
      }

      "produce a Format with SnakeCase" in {
        //#auto-naming-format
        import play.api.libs.json._

        implicit val config = JsonConfiguration(SnakeCase)

        implicit val userFormat: OFormat[PlayUser] = Json.format[PlayUser]
        //#auto-naming-format

        sampleJson2.as[PlayUser] must_=== sampleData2 and {
          Json.toJson(sampleData2) must_=== sampleJson2
        }
      }

      "produce a Reads with SnakeCase" in {
        //#auto-naming-reads
        import play.api.libs.json._

        implicit val config = JsonConfiguration(SnakeCase)

        implicit val userReads: Reads[PlayUser] = Json.reads[PlayUser]
        //#auto-naming-reads

        sampleJson2.as[PlayUser] must_=== sampleData2
      }

      "produce a Format with Custom Naming" in {
        //#auto-custom-naming-format
        import play.api.libs.json._

        object Lightbend extends JsonNaming {
          override def apply(property: String): String = s"lightbend_$property"
        }

        implicit val config = JsonConfiguration(Lightbend)

        implicit val customWrites: OFormat[PlayUser] = Json.format[PlayUser]
        //#auto-custom-naming-format

        sampleJson3.as[PlayUser] must_=== sampleData2 and {
          Json.toJson(sampleData2) must_=== sampleJson3
        }
      }

      "automatically serialize a case class to JSON" in {
        //#auto-case-class-to-JSON
        import play.api.libs.json._

        implicit val residentWrites = Json.writes[Resident]

        val resident = Resident(name = "Fiver", age = 4, role = None)

        val residentJson: JsValue = Json.toJson(resident)
        //#auto-case-class-to-JSON

        residentJson must_=== sampleJson
      }

      "automatically convert JSON to a case class" in {
        //#auto-JSON-to-case-class
        import play.api.libs.json._

        implicit val residentReads = Json.reads[Resident]

        // In a request, a JsValue is likely to come from `request.body.asJson`
        // or just `request.body` if using the `Action(parse.json)` body parser
        val jsonString: JsValue = Json.parse(
          """{
          "name" : "Fiver",
          "age" : 4
        }"""
        )

        val residentFromJson: JsResult[Resident] =
          Json.fromJson[Resident](jsonString)

        residentFromJson match {
          case JsSuccess(r: Resident, path: JsPath) =>
            println("Name: " + r.name)

          case e @ JsError(_) =>
            println("Errors: " + JsError.toJson(e).toString())
        }
        //#auto-JSON-to-case-class

        residentFromJson.get must_=== sampleData
      }
    }

    "for value class" >> {
      "produce a Reads" in {
        //#value-reads
        import play.api.libs.json._

        implicit val idTextReads = Json.valueReads[IdText]
        //#value-reads

        JsString("foo").as[IdText] must_=== (new IdText("foo"))
      }

      "produce a Writes" in {
        //#value-writes
        import play.api.libs.json._

        implicit val idTextWrites = Json.valueWrites[IdText]
        //#value-writes

        Json.toJson(new IdText("bar")) must_=== JsString("bar")
      }

      "produce a Format" in {
        //#value-format
        import play.api.libs.json._

        implicit val idTextFormat = Json.valueFormat[IdText]
        //#value-format

        val id = new IdText("lorem")

        JsString("lorem").as[IdText] must_=== id and {
          Json.toJson(id) must_=== JsString("lorem")
        }
      }
    }

    "automatically convert JSON for a sealed family" in {
      //#trait-representation
      val adminJson = Json.parse("""
        { "_type": "scalaguide.json.ScalaJsonAutomatedSpec.Admin" }
      """)

      val contributorJson = Json.parse("""
        {
          "_type":"scalaguide.json.ScalaJsonAutomatedSpec.Contributor",
          "organization":"Foo"
        }
      """)

      // Each JSON objects is marked with the _type,
      // indicating the fully-qualified name of sub-type
      //#trait-representation

      //#auto-JSON-sealed-trait
      import play.api.libs.json._

      // First provide instance for each sub-types 'Admin' and 'Contributor':
      implicit val adminFormat = OFormat[Admin.type](
        Reads[Admin.type] {
          case JsObject(_) => JsSuccess(Admin)
          case _ => JsError("Empty object expected")
        },
        OWrites[Admin.type] { _ => Json.obj() })

      implicit val contributorFormat = Json.format[Contributor]

      // Finally able to generate format for the sealed family 'Role'
      implicit val roleFormat: OFormat[Role] = Json.format[Role]
      //#auto-JSON-sealed-trait

      def writeAnyRole(role: Role) = Json.toJson(role)

      def readAnyRole(input: JsValue): JsResult[Role] = input.validate[Role]

      val sampleContributor = Contributor("Foo")

      writeAnyRole(Admin) must_=== adminJson and {
        writeAnyRole(sampleContributor) must_=== contributorJson
      } and {
        readAnyRole(adminJson) must_=== JsSuccess(Admin)
      } and {
        readAnyRole(contributorJson) must_=== JsSuccess(sampleContributor)
      }
    }

    "automatically convert custom JSON for a sealed family" in {
      //#trait-custom-representation
      val adminJson = Json.parse("""
        { "admTpe": "admin" }
      """)

      val contributorJson = Json.parse("""
        {
          "admTpe":"contributor",
          "organization":"Foo"
        }
      """)
      //#trait-custom-representation

      //#auto-JSON-custom-trait
      import play.api.libs.json._

      implicit val cfg = JsonConfiguration(
        // Each JSON objects is marked with the admTpe, ...
        discriminator = "admTpe",

        // ... indicating the lower-cased name of sub-type
        typeNaming = JsonNaming { fullName =>
          fullName.drop(39 /* remove pkg */ ).toLowerCase
        })

      // First provide instance for each sub-types 'Admin' and 'Contributor':
      implicit val adminFormat = OFormat[Admin.type](
        Reads[Admin.type] {
          case JsObject(_) => JsSuccess(Admin)
          case _ => JsError("Empty object expected")
        },
        OWrites[Admin.type] { _ => Json.obj() })

      implicit val contributorFormat = Json.format[Contributor]

      // Finally able to generate format for the sealed family 'Role'
      implicit val roleFormat: OFormat[Role] = Json.format[Role]
      //#auto-JSON-custom-trait

      def writeAnyRole(role: Role) = Json.toJson(role)

      def readAnyRole(input: JsValue): JsResult[Role] = input.validate[Role]

      val sampleContributor = Contributor("Foo")

      writeAnyRole(Admin) must_=== adminJson and {
        writeAnyRole(sampleContributor) must_=== contributorJson
      } and {
        readAnyRole(adminJson) must_=== JsSuccess(Admin)
      } and {
        readAnyRole(contributorJson) must_=== JsSuccess(sampleContributor)
      }
    }

    "produce a json object with nulls" in {
      //#auto-writes-null
      import play.api.libs.json._

      implicit val config = JsonConfiguration(optionHandlers = OptionHandlers.WritesNull)
      implicit val residentWrites = Json.writes[Resident]
      //#auto-writes-null

      val resident = Resident(name = "Fiver", age = 4, role = None)

      Json.toJson(resident) must_=== sampleJson4
    }
  }
}
