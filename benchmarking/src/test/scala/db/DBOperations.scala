package db

import java.util.concurrent.atomic.AtomicLong

import org.openjdk.jmh.annotations.{Benchmark, Scope, State}
import org.openjdk.jmh.infra.Blackhole
import bifrost.history._
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}


@OutputTimeUnit(TimeUnit.NANOSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(1)
@Fork(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)

@State(Scope.Thread)
class DBOperations {

  //private val value = new AtomicLong()

  @Benchmark
  def test(bh: Blackhole): Unit = {
    //bh.consume(value.addAndGet(42))
    val sum = 3
  }

  @Benchmark
  def readBlock(bh: Blackhole): Unit = {

  }
}

/*object DBOperations {



  /*@State(Scope.Benchmark)
  class baseState {

    val dataDir: File = new File("/work/Documents/Bifrost/chain-backup/Monon-2019-04-28")
    val blockStorage: LSMStore = new LSMStore(dataDir)
  }*/
}*/