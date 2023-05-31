/*
 * Copyright (C) from 2022 The Play Framework Contributors <https://github.com/playframework>, 2011-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import org.openjdk.jmh.annotations._

import java.util.concurrent.TimeUnit

@Warmup(iterations = 20, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Fork(
  jvmArgsAppend =
    Array("-Xmx350m", "-XX:+HeapDumpOnOutOfMemoryError", "-XX:-BackgroundCompilation", "-XX:-TieredCompilation"),
  value = 1
)
class JsLookupBench {
  @Param(Array("1", "20", "100"))
  var size: Int                  = _
  private var jsObject: JsObject = _

  @Setup def setup(): Unit = {
    jsObject = JsObject(0.until(size).map(i => i.toString -> JsString(s"This is my content for index $i")))
  }

  @Benchmark
  def goodLookup: Option[String] = (jsObject \ (size - 1).toString).asOpt[String]

  @Benchmark
  def badLookup: Option[String] = (jsObject \ (size + 1).toString).asOpt[String]
}
