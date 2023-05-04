/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

case class Employee(
    employeeNumber: Int,
    firstName: String,
    lastName: String,
    city: String,
    country: String,
    tags: Seq[String]
)

object Employee {
  implicit val employeeFormat: Format[Employee] = Json.format[Employee]

  val manualWrites: Writes[Employee] = Writes { e =>
    Json.obj(
      "employeeNumber" -> e.employeeNumber,
      "firstName"      -> e.firstName,
      "lastName"       -> e.lastName,
      "city"           -> e.city,
      "country"        -> e.country,
      "tags"           -> JsArray(e.tags.map(JsString.apply))
    )
  }

  val manualSeqWrites: Writes[Seq[Employee]] = Writes { seq =>
    JsArray(seq.map(manualWrites.writes))
  }
}
