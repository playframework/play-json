/*
 * Copyright (C) 2009-2020 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import org.openjdk.jmh.annotations._

@State(Scope.Benchmark)
class JsonMacros_01_SerializeSimpleCaseClass {

  var employee: Employee = _
  var employeeJson: JsValue = _

  @Setup(Level.Iteration)
  def setup(): Unit = {
    employee = Employee(
      42,
      "Foo",
      "Bar",
      "New York",
      "United States",
      Seq("engineering", "new", "bar")
    )
  }

  @TearDown(Level.Iteration)
  def tearDown(): Unit = {
    assert(employeeJson == Json.parse(
      """ {
        |   "employeeNumber": 42,
        |   "firstName": "Foo",
        |   "lastName": "Bar",
        |   "city": "New York",
        |   "country": "United States",
        |   "tags": ["engineering", "new", "bar"]
        | }
      """.stripMargin))
  }

  @Benchmark
  def toJson(): JsValue = {
    employeeJson = Json.toJson(employee)
    employeeJson
  }

  @Benchmark
  def toJsonManualWrites(): JsValue = {
    employeeJson = Json.toJson(employee)(Employee.manualWrites)
    employeeJson
  }

}
