/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import org.openjdk.jmh.annotations._

import java.util.concurrent.TimeUnit

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
class JsErrorBench {

  private var errors: Seq[(JsPath, Seq[JsonValidationError])] = _

  @Setup(Level.Trial)
  def setup(): Unit = {
    errors = Seq(
      JsPath \ "user" \ "name" ->
        Seq(
          JsonValidationError("error.required")
        ),
      JsPath \ "user" \ "age" ->
        Seq(
          JsonValidationError("error.expected.jsnumber")
        ),
      JsPath \ "address" \ "city" ->
        Seq(
          JsonValidationError("error.required")
        ),
      JsPath \ "email" ->
        Seq(
          JsonValidationError("error.email")
        )
    )
  }

  @Benchmark
  def toJson(): JsObject =
    JsError.toJson(errors)
}
