package db

import java.io.File
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicLong

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import bifrost.forging.ForgingSettings
import bifrost.blocks.{BifrostBlock, BifrostBlockCompanion}
import bifrost.BifrostGenerators
import bifrost.NodeViewModifier.ModifierId
import bifrost.history._

import io.circe.Json
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}


@OutputTimeUnit(TimeUnit.NANOSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(1)
@Fork(1)
@Warmup(iterations = 1)
@Measurement(iterations = 2)
@State(Scope.Benchmark)
class DBOperations extends BifrostGenerators {
  var history: BifrostHistory = generateHistory
  val numOfBlocks: Int = 10
  val numLastBlocks: Int = 5

  val listBlockId: List[ByteArrayWrapper] = (for (i <- 1 to numOfBlocks) yield {
    val oneBlock: BifrostBlock = bifrostBlockGen.sample.get.copy(parentId = history.bestBlockId)
    history = history.append(oneBlock).get._1
    println(s"forging====$i====${ByteArrayWrapper(oneBlock.id)}")
    ByteArrayWrapper(oneBlock.id)
  }).take(numLastBlocks).toList

  val bestBlockIdKey = ByteArrayWrapper(Array.fill(history.storage.storage.keySize)(-1: Byte))
  var storageCurBlockId: ModifierId = history.storage.storage.get(bestBlockIdKey).get.data
  var cacheCurBlockId: ModifierId = history.storage.storage.get(bestBlockIdKey).get.data


  /* Read from storage */
  @Benchmark
  def storageTest {
    for (i <- 1 to numLastBlocks) {
      val currentBlock: BifrostBlock = history.storage.storage.get(ByteArrayWrapper(storageCurBlockId)).map { bw =>
        val bytes = bw.data
        BifrostBlockCompanion.parseBytes(bytes.tail).get
      }.get
      storageCurBlockId = currentBlock.parentId
    }
  }

  /* Read from cache */
  @Benchmark
  def cacheTest {
    for (i <- 1 to numLastBlocks) {
      val currentBlock: BifrostBlock = history.storage.blockCache.getIfPresent(ByteArrayWrapper(cacheCurBlockId)).map {
        bw =>
          val bytes = bw.data
          BifrostBlockCompanion.parseBytes(bytes.tail).get
      }.get
      cacheCurBlockId = currentBlock.parentId
    }
  }

  /* Testing only accessing the storage, not parsing the serialized data */
  @Benchmark
  def storageRead {
    for (id <- listBlockId) {
      val smt = history.storage.storage.get(id)
    }
  }

  /* Testing only accessing the cache, not parsing the serialized data */
  @Benchmark
  def cacheRead {
    for (id <- listBlockId) {
      val smt = history.storage.blockCache.getIfPresent(id)
    }
  }
}



//package db
//
//import java.io.File
//import java.util.concurrent.TimeUnit
//import java.util.concurrent.atomic.AtomicLong
//
//import org.openjdk.jmh.annotations._
//import org.openjdk.jmh.infra.Blackhole
//import bifrost.history._
//import bifrost.forging.ForgingSettings
//import io.circe.Json
//import io.iohk.iodb.LSMStore
//
//
//@OutputTimeUnit(TimeUnit.NANOSECONDS)
//@BenchmarkMode(Array(Mode.AverageTime))
//@Threads(1)
//@Fork(1)
//@Warmup(iterations = 10)
//@Measurement(iterations = 10)
//@State(Scope.Thread)
//class DBOperations {
//
//  private val value = new AtomicLong()
//
//  @Benchmark
//  def test(bh: Blackhole): Unit = {
//    bh.consume(value.addAndGet(42))
//  }
//
//  @Benchmark
//  def readBlock(bh: Blackhole): Unit = {
//
//  }
//}
//
//object DBOperations {
//
//
//
//  @State(Scope.Benchmark)
//  class baseState {
//
//    //TODO Replace hardcoded dir with one from settings
//    val settings: ForgingSettings = new ForgingSettings {
//      override def settingsJSON: Map[String, Json] = settingsFromFile("bench.json")
//    }
//    val dataDir: File = new File(settings.dataDirOpt.get)
//    println(settings.dataDirOpt.getOrElse("Could not find bench.json"))
//    val blockStorage: LSMStore = new LSMStore(dataDir)
//  }
//}