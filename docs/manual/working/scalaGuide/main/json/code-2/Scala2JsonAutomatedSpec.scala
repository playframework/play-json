/*
 * Copyright (C) 2009-2020 Lightbend Inc. <https://www.lightbend.com>
 */

package scalaguide.json

import play.api.libs.json.Json
import org.specs2.mutable.Specification

//#valueClass
final class IdText(val value: String) extends AnyVal
//#valueClass

object Scala2JsonAutomatedSpec {
  //#model1
  sealed trait Role
  case object Admin extends Role
  class Contributor(val organization: String) extends Role {
    override def equals(obj: Any): Boolean = obj match {
      case other: Contributor if obj != null => this.organization == other.organization
      case _                                 => false
    }
  }
  object Contributor {
    def apply(organization: String): Contributor            = new Contributor(organization)
    def unapply(contributor: Contributor): Option[(String)] = Some(contributor.organization)
  }
  //#model1
}

class Scala2JsonAutomatedSpec extends Specification {
  import Scala2JsonAutomatedSpec._

  "Scala 2 JSON automated" should {
    "automatically convert JSON for a sealed family" in {
      //#trait-representation
      val adminJson = Json.parse(s"""
        { "_type": "scalaguide.json.ScalaJsonAutomatedSpec.Admin" }
      """)

      val contributorJson = Json.parse(s"""
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
      implicit val adminFormat = OFormat[Admin.type](Reads[Admin.type] {
        case JsObject(_) => JsSuccess(Admin)
        case _           => JsError("Empty object expected")
      }, OWrites[Admin.type] { _ =>
        Json.obj()
      })

      implicit val contributorFormat: OFormat[Contributor] = Json.format[Contributor]

      // Finally able to generate format for the sealed family 'Role'
      implicit val roleFormat: OFormat[Role] = Json.format[Role]
      //#auto-JSON-sealed-trait

      def writeAnyRole(role: Role) = Json.toJson(role)

      def readAnyRole(input: JsValue): JsResult[Role] = input.validate[Role]

      val sampleContributor = Contributor("Foo")

      writeAnyRole(Admin).must_===(adminJson) and {
        writeAnyRole(sampleContributor).must_===(contributorJson)
      } and {
        readAnyRole(adminJson).must_===(JsSuccess(Admin))
      } and {
        readAnyRole(contributorJson).must_===(JsSuccess(sampleContributor))
      }
    }

    // lampepfl/dotty#7000 No Mirrors for value classes
    "for value class" >> {
      "produce a Reads" in {
        //#value-reads
        import play.api.libs.json._

        implicit val idTextReads: Reads[IdText] = Json.valueReads[IdText]
        //#value-reads

        JsString("foo").as[IdText].must_===(new IdText("foo"))
      }

      "produce a Writes" in {
        //#value-writes
        import play.api.libs.json._

        implicit val idTextWrites: Writes[IdText] = Json.valueWrites[IdText]
        //#value-writes

        Json.toJson(new IdText("bar")).must_===(JsString("bar"))
      }

      "produce a Format" in {
        //#value-format
        import play.api.libs.json._

        implicit val idTextFormat: Format[IdText] = Json.valueFormat[IdText]
        //#value-format

        val id = new IdText("lorem")

        JsString("lorem").as[IdText].must_===(id) and {
          Json.toJson(id).must_===(JsString("lorem"))
        }
      }
    }
  }
}
