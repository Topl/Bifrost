package test.scala.DBOperations

import java.util.concurrent.atomic.AtomicLong
import java.util.concurrent.TimeUnit
//import bifrost.history._
//import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.annotations.Scope
import org.openjdk.jmh.annotations.State
import org.openjdk.jmh.infra.Blackhole

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



  /*@State(Scope.Benchmark)
  class baseState {

    val dataDir: File = new File("/work/Documents/Bifrost/chain-backup/Monon-2019-04-28")
    val blockStorage: LSMStore = new LSMStore(dataDir)
  }*/
}