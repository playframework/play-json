/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import org.openjdk.jmh.annotations._

@State(Scope.Benchmark)
class OWritesBench {

  private var employee: Employee = _

  private val employeeWritesFromFields = new OWrites.OWritesFromFields[Employee] {
    override def writeFields(
      fieldsMap: java.util.LinkedHashMap[String, JsValue],
      e: Employee
    ): Unit = {
      fieldsMap.put("employeeNumber", JsNumber(e.employeeNumber))
      fieldsMap.put("firstName", JsString(e.firstName))
      fieldsMap.put("lastName", JsString(e.lastName))
      fieldsMap.put("city", JsString(e.city))
      fieldsMap.put("country", JsString(e.country))
      fieldsMap.put("tags", JsArray(e.tags.map(JsString.apply)))
    }
  }

  private var employeeIdMap: Map[String, String] = _

  private var scoreMap: Map[Int, Double] = _

  @Setup(Level.Trial)
  def setup(): Unit = {
    employee = Employee(
      42,
      "Foo",
      "Bar",
      "New York",
      "United States",
      Seq("engineering", "new", "bar")
    )

    employeeIdMap = Map(
      "firstName" -> employee.firstName,
      "lastName" -> employee.lastName,
      "city" -> employee.city,
      "country" -> employee.country)

    scoreMap = (1 to 100).map(i => i -> (i / 10D)).toMap
  }

  // ---

  @Benchmark
  def writesFromFields(): JsObject =
    employeeWritesFromFields.writes(employee)

  @Benchmark
  def writesFromGenericMap(): JsObject =
    Writes.genericMapWrites.writes(employeeIdMap)

  @Benchmark
  def writesFromKeyMap(): JsObject =
    Writes.keyMapWrites.writes(scoreMap)
}
