package co.topl.nodeView.history

import co.topl.consensus.ConsensusVariables.ConsensusParams
import co.topl.consensus.NxtLeaderElection
import co.topl.db.LDBVersionedStore
import co.topl.modifier.block.Block
import co.topl.nodeView.{CacheLayerKeyValueStore, LDBKeyValueStore}
import co.topl.utils.GeneratorOps.GeneratorOps
import co.topl.utils.{Int128, NodeGenerators}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class StorageCacheSpec extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers with NodeGenerators {

  var history: History = _

  override def beforeAll(): Unit = {
    super.beforeAll()

    history = generateHistory()
  }

  property("The genesis block is stored in cache") {
    val genesisBlockId = Array.fill(33)(-1: Byte)

    history.storage.keyValueStore
      .asInstanceOf[CacheLayerKeyValueStore]
      .cache
      .getIfPresent(new CacheLayerKeyValueStore.WrappedBytes(genesisBlockId)) shouldEqual history.storage.keyValueStore
      .get(genesisBlockId)
  }

  property("Cache should invalidate all entry when it's rolled back in storage") {
    val bestBlockIdKey = Array.fill(33)(-1: Byte)

    /* Append a new block, make sure it is updated in cache, then drop it */
    val fstBlock: Block = blockCurve25519Gen.sampleFirst().copy(parentId = history.bestBlockId)
    history = history
      .append(fstBlock, ConsensusParams(10000000, history.bestBlock.difficulty, 0L, history.bestBlock.height))
      .get
      ._1

    history.storage.keyValueStore
      .asInstanceOf[CacheLayerKeyValueStore]
      .cache
      .getIfPresent(new CacheLayerKeyValueStore.WrappedBytes(bestBlockIdKey)) should not be null

    history = history.drop(fstBlock.id)

    /* After dropping the new block, the best block should be its parent block, the genesis block */
    /* The block Id for best block in cache should be invalidated */
    history.storage.keyValueStore.asInstanceOf[CacheLayerKeyValueStore].cache.asMap().size() shouldBe 0

    /* Append multiple times */
    forAll(blockCurve25519Gen) { blockTemp =>
      val block: Block = blockTemp.copy(parentId = history.bestBlockId)

      history = history
        .append(block, ConsensusParams(10000000, history.bestBlock.difficulty, 0L, history.bestBlock.height))
        .get
        ._1
    }

    /* Drop all new blocks */
    var continue = true
    while (continue) {
      val height = history.height
      if (height <= 2) continue = false
      history = history.drop(history.bestBlockId)
    }
    /* Same checks as above */
    val cache =
      history.storage.keyValueStore.asInstanceOf[CacheLayerKeyValueStore].cache

    cache.asMap().size() shouldBe 0

    /* Maybe also check other entries like blockScore */
  }

  property("The new block updated is stored in cache") {

    forAll(blockCurve25519Gen) { blockTemp =>
      val block: Block = blockTemp.copy(parentId = history.bestBlockId)

      history = history
        .append(block, ConsensusParams(10000000, history.bestBlock.difficulty, 0L, history.bestBlock.height))
        .get
        ._1
      history.storage.keyValueStore
        .asInstanceOf[CacheLayerKeyValueStore]
        .cache
        .getIfPresent(new CacheLayerKeyValueStore.WrappedBytes(block.id.getIdBytes)) shouldEqual
      history.storage.keyValueStore.get(block.id.getIdBytes)
    }
  }

  /* -----This test need to be done with smaller cacheSize or it will take very long to append enough entries----- */
  /* --------This test is commented out, change cacheSize in test.conf if we need to test this again------- */
  /*
  private val cacheSize: Int = settings.cacheSize

  property("Appending more entries than the maximum cache size will drop a portion of existing cache") {

    /* Append one block */
    val fstBlock: Block = BlockGen.sample.get.copy(parentId = history.bestBlockId)
    history = history.append(fstBlock).get._1

    history.storage.blockCache.getIfPresent(ByteArrayWrapper(fstBlock.id.hashBytes)) should not be null

    /* Append a number of new blocks, so that we store more entries than the cache size limit */
    /* Assuming an average new block creates more than 50 entries */
    val numOfBlocks:Int = cacheSize / 50
    (1 to numOfBlocks) foreach { _ =>
      val oneBlock:Block = BlockGen.sample.get.copy(parentId = history.bestBlockId)
      history = history.append(oneBlock).get._1
    }

    history.storage.blockCache.getIfPresent(ByteArrayWrapper(fstBlock.id.hashBytes)) shouldBe null
  }
   */

  property("blockLoader should correctly return a block from storage not found in cache") {
    val block: Block = blockCurve25519Gen.sampleFirst().copy(parentId = history.bestBlockId)
    history = history
      .append(block, ConsensusParams(10000000, history.bestBlock.difficulty, 0L, history.bestBlock.height))
      .get
      ._1

    history.storage.keyValueStore.asInstanceOf[CacheLayerKeyValueStore].cache.invalidateAll()
    history.modifierById(block.id).get should not be None
  }

  override def generateHistory(genesisBlock: Block): History = {
    import java.io.File
    import scala.concurrent.duration._
    import scala.util.Random
    val dataDir = s"/tmp/bifrost/test-data/test-${Random.nextInt(10000000)}"

    val iFile = new File(s"$dataDir/blocks")
    iFile.mkdirs()
    val blockStorage = new LDBVersionedStore(iFile, 100)
    val storage =
      new Storage(new CacheLayerKeyValueStore(new LDBKeyValueStore(blockStorage), 10.minutes, 20000))
    // we don't care about validation here
    val validators = Seq()

    var history = new History(storage, BlockProcessor(1024), validators)

    history = history.append(genesisBlock, ConsensusParams(Int128(10000000), 1000000000000000000L, 0L, 0L)).get._1
    assert(history.modifierById(genesisBlock.id).isDefined)
    history
  }
}
