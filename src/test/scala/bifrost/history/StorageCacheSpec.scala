package bifrost.history


import bifrost.BifrostGenerators
import bifrost.NodeViewModifier.ModifierId
import bifrost.blocks.{BifrostBlock, BifrostBlockCompanion}
import com.typesafe.config.{Config, ConfigFactory}
import io.iohk.iodb.ByteArrayWrapper
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class StorageCacheSpec extends PropSpec
  with ScalaCheckPropertyChecks
  with Matchers
  with BifrostGenerators {

  var history: BifrostHistory = generateHistory

  private val conf: Config = ConfigFactory.load("application")
  private val expireTime: Int = conf.getInt("cache.expireTime")
  private val cacheSize: Int = conf.getInt("cache.cacheSize")

  property("The genesis block is stored in cache") {
    val genesisBlockId = ByteArrayWrapper(Array.fill(history.storage.storage.keySize)(-1: Byte))

    history.storage.blockCache.getIfPresent(genesisBlockId) shouldEqual history.storage.storage.get(genesisBlockId)
  }

  property("Cache should invalidate the entry when it's rolled back in storage") {
    val bestBlockIdKey = ByteArrayWrapper(Array.fill(history.storage.storage.keySize)(-1: Byte))
    val genesisBlockId = history.storage.storage.get(bestBlockIdKey).get

    /* Append a new block, make sure it is updated in cache, then drop it */
    val fstBlock:BifrostBlock = bifrostBlockGen.sample.get.copy(parentId = history.bestBlockId)
    history = history.append(fstBlock).get._1

    history.storage.blockCache.getIfPresent(bestBlockIdKey) should not be null

    history = history.drop(fstBlock.id)

    /* After dropping the new block, the best block should be its parent block, the genesis block */
    /* The block Id for best block in cache should be invalidated */
    history.storage.blockCache.getIfPresent(bestBlockIdKey) shouldBe null

    /* We still get history.bestBlockId because it uses .get() which access the LSMStore if nothing is found in cache */
    history.storage.blockCache.getIfPresent(ByteArrayWrapper(history.bestBlockId)) shouldEqual
      history.storage.storage.get(genesisBlockId)
    history.storage.blockCache.getIfPresent(bestBlockIdKey) shouldEqual
      history.storage.storage.get(bestBlockIdKey)

    /* Append multiple times */
    forAll(bifrostBlockGen) { blockTemp =>
      val block:BifrostBlock = blockTemp.copy(parentId = history.bestBlockId)

      history = history.append(block).get._1
    }

    /* Drop all new blocks */
    val height: Int = history.height.toInt
    (2 to height) foreach { _ =>
      history = history.drop(history.bestBlockId)
    }
    /* Same checks as above */
    history.storage.blockCache.getIfPresent(bestBlockIdKey) shouldBe null
    history.storage.blockCache.getIfPresent(ByteArrayWrapper(history.bestBlockId)) shouldEqual
      history.storage.storage.get(genesisBlockId)
    history.storage.blockCache.getIfPresent(bestBlockIdKey) shouldEqual
      history.storage.storage.get(bestBlockIdKey)
    /* Maybe also check other entries like blockScore */
  }

  property("The new block updated is stored in cache") {

    forAll(bifrostBlockGen) { blockTemp =>
      val block:BifrostBlock = blockTemp.copy(parentId = history.bestBlockId)

      history = history.append(block).get._1
      history.storage.blockCache.getIfPresent(ByteArrayWrapper(block.id)) shouldEqual
        history.storage.storage.get(ByteArrayWrapper(block.id))
    }
  }

  property("Appending more entries than the maximum cache size will drop a portion of existing cache") {
    /* Append one block */
    val fstBlock: BifrostBlock = bifrostBlockGen.sample.get.copy(parentId = history.bestBlockId)
    history = history.append(fstBlock).get._1

    history.storage.blockCache.getIfPresent(ByteArrayWrapper(fstBlock.id)) should not be null

    /* Append a number of new blocks, so that we store more entries than the cache size limit */
    /* Assuming an average new block creates more than 50 entries */
    val numOfBlocks:Int = cacheSize / 50
    (1 to numOfBlocks) foreach { _ =>
      val oneBlock:BifrostBlock = bifrostBlockGen.sample.get.copy(parentId = history.bestBlockId)
      history = history.append(oneBlock).get._1
    }

    history.storage.blockCache.getIfPresent(ByteArrayWrapper(fstBlock.id)) shouldBe null
  }

  /* Make sure the expireTime in application.conf is set to a small value for shorter wait time */
  /* eg. expireTime = 2000 */
  property("Cache entries should expire if it is not accessed after a certain time") {
    /* Wait for 2 seconds more than the expiration time */
    /* Wait time is calculated assuming expireTime uses minutes */
    assert(expireTime < 5000)
    val timeToWait = expireTime + 2000
    val fstBlock:BifrostBlock = bifrostBlockGen.sample.get.copy(parentId = history.bestBlockId)

    history = history.append(fstBlock).get._1

    history.storage.blockCache.getIfPresent(ByteArrayWrapper(fstBlock.id)) should not be null

    Thread.sleep(timeToWait)

    history.storage.blockCache.getIfPresent(ByteArrayWrapper(fstBlock.id)) shouldBe null
  }

  /* TODO: Benchmarking */
//  property("Load 1000 blocks and read the last 50 and compare the performance between cache and storage") {
//    val numOfBlocks:Int = 55
//    for (i <- 1 to numOfBlocks) {
//      val oneBlock:BifrostBlock = bifrostBlockGen.sample.get.copy(parentId = history.bestBlockId)
//      history = history.append(oneBlock).get._1
//      println(s"forging====$i====${ByteArrayWrapper(oneBlock.id)}")
//    }
//
//    val bestBlockIdKey = ByteArrayWrapper(Array.fill(history.storage.storage.keySize)(-1: Byte))
//    var storageCurBlockId: ModifierId = history.storage.storage.get(bestBlockIdKey).get.data
//    var cacheCurBlockId: ModifierId = history.storage.storage.get(bestBlockIdKey).get.data
//    var tempCurBlockId: ModifierId = history.storage.storage.get(bestBlockIdKey).get.data
//
//    /* Read from storage */
//    val t1 = System.currentTimeMillis
//    for (i <- 1 to 50) {
//      val currentBlock: BifrostBlock = history.storage.storage.get(ByteArrayWrapper(storageCurBlockId)).map { bw =>
//        val bytes = bw.data
//        BifrostBlockCompanion.parseBytes(bytes.tail).get
//      }.get
//      storageCurBlockId = currentBlock.parentId
//    }
//    val storageDuration = (System.currentTimeMillis - t1) / 1e3d
//
//    /* Read from cache */
//    val t2 = System.currentTimeMillis
//    for (i <- 1 to 50) {
//      val currentBlock: BifrostBlock = history.storage.blockCache.getIfPresent(ByteArrayWrapper(cacheCurBlockId)).map {
//        bw =>
//          val bytes = bw.data
//          BifrostBlockCompanion.parseBytes(bytes.tail).get
//      }.get
//      cacheCurBlockId = currentBlock.parentId
//    }
//    val cacheDuration = (System.currentTimeMillis - t2) / 1e3d
//
//    /* Testing only accessing the storage and cache, not parsing the serialized data */
//    val t3 = System.currentTimeMillis
//    for (_ <- 1 to 10000) {
//      val smt = history.storage.storage.get(ByteArrayWrapper(tempCurBlockId))
//    }
//    val tempStorageDuration = (System.currentTimeMillis - t3) / 1e3d
//    println(s"temp storage duration:$tempStorageDuration")
//
//    val t4 = System.currentTimeMillis
//    for (_ <- 1 to 10000) {
//      val smt = history.storage.blockCache.getIfPresent(ByteArrayWrapper(tempCurBlockId))
//    }
//    val tempCacheDuration = (System.currentTimeMillis - t4) / 1e3d
//    println(s"temp cache duration:$tempCacheDuration")
//
//
//    println(s"cache:$cacheDuration---storage:$storageDuration")
//
//    /* This may not be the case */
//    (cacheDuration < storageDuration) shouldBe true
//  }
}