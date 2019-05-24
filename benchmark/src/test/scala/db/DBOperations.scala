package db

import java.io.File
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicLong

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import bifrost.history._
import bifrost.forging.ForgingSettings
import io.circe.Json
import io.iohk.iodb.LSMStore


@OutputTimeUnit(TimeUnit.NANOSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(1)
@Fork(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@State(Scope.Thread)
class DBOperations {

  private val value = new AtomicLong()

  @Benchmark
  def test(bh: Blackhole): Unit = {
    bh.consume(value.addAndGet(42))
  }

  @Benchmark
  def readBlock(bh: Blackhole): Unit = {

  }
}

object DBOperations {



  @State(Scope.Benchmark)
  class baseState {

    //TODO Replace hardcoded dir with one from settings
    val settings: ForgingSettings = new ForgingSettings {
      override def settingsJSON: Map[String, Json] = settingsFromFile("bench.json")
    }
    val dataDir: File = new File(settings.dataDirOpt.get)
    println(settings.dataDirOpt.getOrElse("Could not find bench.json"))
    val blockStorage: LSMStore = new LSMStore(dataDir)
  }
}