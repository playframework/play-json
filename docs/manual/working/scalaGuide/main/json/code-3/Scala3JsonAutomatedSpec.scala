/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package scalaguide.json

import org.specs2.mutable.Specification
import play.api.libs.json._

object Scala3JsonAutomatedSpec {
  //#reads-model
  case class Resident(
    name: String,
    age: Int,
    specialism: Option[String]
  ) derives Reads
  //#reads-model

  val residentJson = Json.parse(
    """{
      "name" : "Fiver",
      "age" : 4
    }"""
  )

  val sampleResident = Resident("Fiver", 4, None)

  //#reads-trait
  sealed trait Role derives Reads
  case object Admin                            extends Role derives Reads
  case class Contributor(organization: String) extends Role derives Reads
  //#reads-trait

  val contributorJson = Json.obj("_type" -> "scalaguide.json.Scala3JsonAutomatedSpec.Contributor", "organization" -> "Foo")
  val sampleContributor = Contributor("Foo")
}

class Scala3JsonAutomatedSpec extends Specification {
  import Scala3JsonAutomatedSpec._

  "Scala 3 JSON automated" should {
    "for case class" >> {
      "derive a Reads" in {
        residentJson.as[Resident].must_===(sampleResident)
      }
    }
    "for trait" >> {
      "derive a Reads if every subclass derives Reads" in {
        contributorJson.as[Role].must_===(sampleContributor)
      }
    }
  }
}
