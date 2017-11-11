/**
 * Copyright (C) 2009-2017 Lightbend Inc. <https://www.lightbend.com>
 */
package play.api.libs.json

import org.openjdk.jmh.annotations._

@State(Scope.Benchmark)
class JsonLookups_02_LookupDef {

  var defValue: JsValue = _

  @Setup(Level.Iteration)
  def setup(): Unit = {
    defValue = Json.obj(
      "foobar01" -> "test",
      "foobar02" -> "test",
      "foobar03" -> "test",
      "foobar04" -> "test",
      "foobar05" -> "test"
    )
  }

  @Benchmark
  def asOpt(): Option[String] = (defValue \ "foobar03").asOpt[String]

  @Benchmark
  def as(): String = (defValue \ "foobar03").as[String]

  @Benchmark
  def validate(): JsResult[String] = (defValue \ "foobar03").validate[String]

  @Benchmark
  def validateOpt(): JsResult[Option[String]] = (defValue \ "foobar03").validateOpt[String]

}
