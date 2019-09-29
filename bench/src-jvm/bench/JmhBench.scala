package bench

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import upickle.Common

/**
  * ==Run with==
  * mill bench.jvm.runJmh JmhBench
  */
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Fork(
  jvmArgsAppend = Array(
//    "-XX:+UnlockCommercialFeatures",
//    "-XX:+FlightRecorder",
//    "-XX:StartFlightRecording=duration=5s,filename=recording.jfr,settings=profile",
    "-Xmx350m",
    "-XX:+HeapDumpOnOutOfMemoryError"
  ),
  value = 1
)
class JmhBench {

  @Benchmark
  def writeJson(bh: Blackhole): Unit = {
    bh.consume(upickle.default.write(Common.benchmarkSampleData))
  }

  @Benchmark
  def writeMsgPack(bh: Blackhole): Unit = {
    bh.consume(upickle.default.writeBinary(Common.benchmarkSampleData))
  }

  @Benchmark
  def readJson(bh: Blackhole): Unit = {
    bh.consume(upickle.default.read[Common.Data](Common.benchmarkSampleJson))
  }

  @Benchmark
  def readMsgPack(bh: Blackhole): Unit = {
    bh.consume(upickle.default.readBinary[Common.Data](Common.benchmarkSampleMsgPack))
  }
}
