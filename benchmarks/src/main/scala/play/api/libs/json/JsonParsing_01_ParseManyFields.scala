/*
 * Copyright (C) 2009-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import org.openjdk.jmh.annotations._

/**
 * @see https://github.com/playframework/play-json/pull/193
 */
@State(Scope.Benchmark)
class JsonParsing_01_ParseManyFields {
  @Param(Array("10", "100", "1000", "10000", "100000"))
  var n: Int = 100

  var stringToParse: String = _

  case class Example(s: String)
  object Example {
    implicit val reads = Json.reads[Example]
  }

  @Setup
  def setup(): Unit = {
    val value = "42"
    stringToParse = HashCodeCollider.zeroHashCodeStrings
      .take(n)
      .mkString("""{"s":"s","""", s"""":$value,"""", s"""":$value,"i":1}""")
  }

  @Benchmark
  def parseObject(): JsValue = {
    Json.parse(stringToParse)
  }

  @Benchmark
  def parseObjectAs(): Example = {
    Json.parse(stringToParse).as[Example]
  }

  @Benchmark
  def parseObjectLookup(): JsValue = {
    (Json.parse(stringToParse) \ "s").as[JsString]
  }

  @Benchmark
  def parseObjectValue(): Option[JsValue] = {
    Json.parse(stringToParse).as[JsObject].value.get("s")
  }
}
