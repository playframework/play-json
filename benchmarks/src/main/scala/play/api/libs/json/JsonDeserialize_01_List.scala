/*
 * Copyright (C) 2009-2019 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import org.openjdk.jmh.annotations._

@State(Scope.Benchmark)
class JsonDeserialize_01_List {

  var employees: Seq[Employee] = _
  var employeesJson: JsValue = _

  @Setup(Level.Iteration)
  def setup(): Unit = {
    val employees: Seq[Employee] = (1 to 100) map { id =>
      Employee(
        id,
        s"Foo$id",
        s"Bar$id",
        "New York",
        "United States",
        Seq("a", "b", "c")
      )
    }
    employeesJson = Json.toJson(employees)
  }

  @TearDown(Level.Iteration)
  def tearDown(): Unit = {

  }

  @Benchmark
  def jsonAs(): JsValue = {
    employees = employeesJson.as[Seq[Employee]]
    employeesJson
  }
}
