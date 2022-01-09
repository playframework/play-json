package play.api.libs.json

import org.openjdk.jmh.annotations._

import java.util.concurrent.TimeUnit

/**
 * ==Quick Run==
 * benchmarks / Jmh / run .*JsObjectBench
 *
 * ==Profile with Flight Recorder==
 * benchmarks / Jmh / run -prof jfr .*JsObjectBench
 *
 * ==Jmh Visualizer Report==
 * benchmarks / Jmh / run -f5 -prof gc -rf json -rff JsObjectBench-results.json .*JsObjectBench
 *
 * ==Sample Results==
 * {{{
 * Benchmark                    (size)   Mode  Cnt       Score       Error   Units
 * JsObjectBench.oEqualsCopy        15  thrpt    5    4386.757 ±   124.675  ops/ms
 * JsObjectBench.oEqualsEq          15  thrpt    5  430769.750 ± 43891.912  ops/ms
 * JsObjectBench.oEqualsNe          15  thrpt    5  321636.207 ± 23899.623  ops/ms
 * JsObjectBench.oHashCodeWarm      15  thrpt    5    4024.849 ±   276.276  ops/ms
 * }}}
 *
 * @see https://github.com/ktoso/sbt-jmh
 */
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
class JsObjectBench {

  @Param(Array("1", "15"))
  var size: Int                       = _
  private var jsObject: JsObject      = _
  private var jsObjectClone: JsObject = _
  private var jsObjectCopy: JsObject  = _

  @Setup def setup(): Unit = {
    jsObject = JsObject(0.until(size).map(i => i.toString -> JsTrue))
    jsObjectClone = JsObject(jsObject.underlying)
    jsObjectCopy = Json.parse(jsObject.toString).as[JsObject]
  }

  @Benchmark def oEqualsEq     = jsObject == jsObject
  @Benchmark def oEqualsNe     = jsObject == jsObjectClone
  @Benchmark def oEqualsCopy   = jsObject == jsObjectCopy
  @Benchmark def oHashCodeWarm = jsObject.hashCode()
}
