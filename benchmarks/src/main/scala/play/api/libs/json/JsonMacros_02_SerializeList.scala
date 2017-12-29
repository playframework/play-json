package play.api.libs.json

import org.openjdk.jmh.annotations._

@State(Scope.Benchmark)
class JsonMacros_02_SerializeList {

  var employees: Seq[Employee] = _
  var employeesJson: JsValue = _

  @Setup(Level.Iteration)
  def setup(): Unit = {
    employees = (1 to 100) map { id =>
      Employee(
        id,
        s"Foo$id",
        s"Bar$id",
        "New York",
        "United States",
        Seq("a", "b", "c")
      )
    }
  }

  @TearDown(Level.Iteration)
  def tearDown(): Unit = {

  }

  @Benchmark
  def toJson(): JsValue = {
    employeesJson = Json.toJson(employees)
    employeesJson
  }

  @Benchmark
  def toJsonManualWrites(): JsValue = {
    employeesJson = Json.toJson(employees)(Employee.manualSeqWrites)
    employeesJson
  }
}
