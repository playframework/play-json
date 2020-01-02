/*
 * Copyright (C) 2009-2020 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import org.openjdk.jmh.annotations._

@State(Scope.Benchmark)
class JsonDeserialize_02_Nullable {

  case class NullableFields(field1: Option[String], field2: Option[String], field3: Option[String])

  private val json = Json.obj("field1" -> "value1", "field2" -> "value2")
  private var result: NullableFields = _
  private implicit val nullableFields: Reads[NullableFields] = Json.reads

  @Benchmark
  def jsonAs(): Unit = {
    result = json.as[NullableFields]
  }

  @TearDown(Level.Iteration)
  def tearDown(): Unit = {
    assert(result == NullableFields(Some("value1"), Some("value2"), None))
  }
}

