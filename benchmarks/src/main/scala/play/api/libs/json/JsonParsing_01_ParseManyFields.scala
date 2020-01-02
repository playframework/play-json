/*
 * Copyright (C) 2009-2020 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import org.openjdk.jmh.annotations._

@State(Scope.Benchmark)
class JsonParsing_01_ParseManyFields {
  @Param(Array("10", "100", "1000", "10000", "100000"))
  var n: Int = 100

  var stringToParse: String = _

  @Setup
  def setup(): Unit = {
    val value = "42"
    stringToParse = HashCodeCollider.zeroHashCodeStrings.take(n)
      .mkString("""{"s":"s","""", s"""":$value,"""", s"""":$value,"i":1}""")
  }

  @Benchmark
  def parseObject(): Unit = {
    Json.parse(stringToParse)
  }
}
