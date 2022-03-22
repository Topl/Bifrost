package co.topl.nodeView.history

import co.topl.modifier.block.Block
import co.topl.nodeView.{CacheLayerKeyValueStore, ValidTransactionGenerators}
import co.topl.utils.{NodeGenerators, ProtocolVersionHelper, TestSettings}
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.concurrent.duration.DurationInt

class StorageCacheSpec
    extends AnyPropSpec
    with ScalaCheckDrivenPropertyChecks
    with NodeGenerators
    with ProtocolVersionHelper
    with Matchers
    with ValidTransactionGenerators {

  property("The genesis block is stored in cache") {
    withTestHistory { history =>
      val genesisBlockId = Array.fill(33)(-1: Byte)

      history.storage.keyValueStore
        .asInstanceOf[CacheLayerKeyValueStore]
        .cache
        .getIfPresent(
          new CacheLayerKeyValueStore.WrappedBytes(genesisBlockId)
        ) shouldEqual history.storage.keyValueStore
        .get(genesisBlockId)
    }
  }

  property("Cache should invalidate a single entry when it's rolled back in storage") {
    withTestHistory { history =>
      forAll(blockCurve25519Gen) { block =>
        val bestBlockIdKey = Array.fill(33)(-1: Byte)

        val historyAddBlock = history.append(block, Seq()).get._1

        historyAddBlock.storage.keyValueStore
          .asInstanceOf[CacheLayerKeyValueStore]
          .cache
          .getIfPresent(new CacheLayerKeyValueStore.WrappedBytes(bestBlockIdKey)) should not be null

        val historyDropBlock = historyAddBlock.drop(block.id)

        /* After dropping the new block, the best block should be its parent block, the genesis block */
        /* The block Id for best block in cache should be invalidated */
        historyDropBlock.storage.keyValueStore.asInstanceOf[CacheLayerKeyValueStore].cache.asMap().size() shouldBe 0
      }
    }
  }

  property("Cache should invalidate multiple entries when it's rolled back in storage") {
    withTestHistory { history =>
      forAll(Gen.nonEmptyListOf(blockCurve25519Gen)) { blocks =>
        val bestBlockIdKey = Array.fill(33)(-1: Byte)

        val historyAddBlock = blocks.foldLeft(history) { (accHistory, newBlock) =>
          accHistory.append(newBlock, Seq()).get._1
        }

        historyAddBlock.storage.keyValueStore
          .asInstanceOf[CacheLayerKeyValueStore]
          .cache
          .getIfPresent(new CacheLayerKeyValueStore.WrappedBytes(bestBlockIdKey)) should not be null

        val historyDropBlock = blocks.foldLeft(history) { (accHistory, block) =>
          accHistory.drop(block.id)
        }

        historyDropBlock.storage.keyValueStore.asInstanceOf[CacheLayerKeyValueStore].cache.asMap().size() shouldBe 0
      }
    }
  }

  property("The new block updated is stored in cache") {
    withTestHistory { history =>
      forAll(blockCurve25519Gen) { blockTemp =>
        val block: Block = blockTemp.copy(parentId = history.bestBlockId)

        val updatedHistory = history.append(block, Seq()).get._1

        val blockInCache =
          updatedHistory.storage.keyValueStore
            .asInstanceOf[CacheLayerKeyValueStore]
            .cache
            .getIfPresent(new CacheLayerKeyValueStore.WrappedBytes(block.id.getIdBytes))

        blockInCache shouldEqual updatedHistory.storage.keyValueStore.get(block.id.getIdBytes)
      }
    }
  }

  property("Appending more entries than the maximum cache size will drop a portion of existing cache") {
    forAll(genesisBlockGen, Gen.listOfN(10, blockCurve25519Gen)){ (genesisBlock, blocks) =>
      def customCacheHistory(cacheSize: Int): History = {
        val cacheStore = new CacheLayerKeyValueStore(new InMemoryKeyValueStore, 30.seconds, cacheSize)
        val storage = new Storage(cacheStore)
        val history = new History(storage, TineProcessor(1024))
        history.append(genesisBlock, Seq()).get._1
      }

      val history = customCacheHistory(5)
      val historyWithOneBlock = history.append(blocks.head, Seq()).get._1
      val historyWithTooManyBlocks = blocks.foldLeft(history) { (accHistory, newBlock) =>
        accHistory.append(newBlock, Seq()).get._1
      }

      historyWithOneBlock.storage.keyValueStore
        .asInstanceOf[CacheLayerKeyValueStore]
        .cache
        .getIfPresent(new CacheLayerKeyValueStore.WrappedBytes(blocks.head.id.getIdBytes)) should not be null

      historyWithTooManyBlocks.storage.keyValueStore
        .asInstanceOf[CacheLayerKeyValueStore]
        .cache
        .getIfPresent(new CacheLayerKeyValueStore.WrappedBytes(blocks.head.id.getIdBytes)) shouldBe null
    }
  }

  property("blockLoader should correctly return a block from storage not found in cache") {
    withTestHistory { history =>
      forAll(blockCurve25519Gen){ block =>
        val updatedHistory = history.append(block, Seq()).get._1
        updatedHistory.storage.keyValueStore.asInstanceOf[CacheLayerKeyValueStore].cache.invalidateAll()
        updatedHistory.modifierById(block.id).get should not be None
      }
    }
  }

  private def withTestHistory(test: History => Unit): Unit =
    test {
      val genesisBlock = genesisBlockGen.sample.get

      val underlyingStore = new InMemoryKeyValueStore
      val cacheStore = new CacheLayerKeyValueStore(underlyingStore, 10.seconds, 512)
      val storage = new Storage(cacheStore)
      val history = new History(storage, TineProcessor(1024))
      history.append(genesisBlock, Seq()).get._1
    }
}
