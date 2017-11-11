/**
 * Copyright (C) 2009-2017 Lightbend Inc. <https://www.lightbend.com>
 */
package play.api.libs.json

import org.openjdk.jmh.annotations._

@State(Scope.Benchmark)
class JsonLookups_01_LookupUndef {

  var undefValue: JsValue = _

  @Setup(Level.Iteration)
  def setup(): Unit = {
    undefValue = Json.obj(
      "foobar01" -> "test",
      "foobar02" -> "test",
      "foobar03" -> "test",
      "foobar04" -> "test",
      "foobar05" -> "test"
    )
  }

  @Benchmark
  def asOpt(): Option[String] = (undefValue \ "missing").asOpt[String]

  @Benchmark
  def as(): String = {
    try {
      (undefValue \ "missing").as[String]
    } catch {
      case e: JsResultException => e.getMessage
    }
  }

  @Benchmark
  def validate(): JsResult[String] = (undefValue \ "missing").validate[String]

  @Benchmark
  def validateOpt(): JsResult[Option[String]] = (undefValue \ "missing").validateOpt[String]

}
