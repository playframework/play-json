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
  def retrieve(): Option[String] = (undefValue \ "missing").asOpt[String]

}
