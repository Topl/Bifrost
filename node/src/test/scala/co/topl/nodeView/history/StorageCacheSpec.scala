package co.topl.nodeView.history

import co.topl.consensus.ValidBlockchainGenerator
import co.topl.modifier.block.Block
import co.topl.nodeView.CacheLayerKeyValueStore
import co.topl.utils.{InMemoryKeyRingTestHelper, NodeGenerators, TestSettings}
import org.scalacheck.Gen
import org.scalatest.OptionValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.concurrent.duration.DurationInt

class StorageCacheSpec
    extends AnyPropSpec
    with ScalaCheckDrivenPropertyChecks
    with ValidBlockchainGenerator
    with NodeGenerators
    with InMemoryKeyRingTestHelper
    with TestSettings
    with OptionValues
    with Matchers {

  private val bestBlockIdKey = Array.fill(33)(-1: Byte)

  property("The genesis block is stored in cache") {
    withTestHistory(genesisBlockGen.sample.get) { history =>
      history.storage.keyValueStore
        .asInstanceOf[CacheLayerKeyValueStore]
        .cache
        .getIfPresent(
          new CacheLayerKeyValueStore.WrappedBytes(bestBlockIdKey)
        ) shouldEqual history.storage.keyValueStore
        .get(bestBlockIdKey)
    }
  }

  property("Cache should invalidate a single entry when it's rolled back in storage") {
    forAll(chainGen(2)) { chain =>
      withTestHistory(chain.head.block) { history =>
        val block = chain.tail.head
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
    forAll(chainGen(2)) { chain =>
      withTestHistory(chain.head.block) { history =>
        val historyAddBlock = chain.tail.foldLeft(history) { (accHistory, newBlock) =>
          accHistory.append(newBlock, Seq()).get._1
        }

        historyAddBlock.storage.keyValueStore
          .asInstanceOf[CacheLayerKeyValueStore]
          .cache
          .getIfPresent(new CacheLayerKeyValueStore.WrappedBytes(bestBlockIdKey)) should not be null

        val historyDropBlock = chain.tail.reverse.foldLeft(history) { (accHistory, block) =>
          accHistory.drop(block.id)
        }

        historyDropBlock.storage.keyValueStore.asInstanceOf[CacheLayerKeyValueStore].cache.asMap().size() shouldBe 0
      }
    }
  }

  property("The new block updated is stored in cache") {
    forAll(chainGen(2)) { chain =>
      withTestHistory(chain.head.block) { history =>
        val block: Block = chain.tail.head
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
    forAll(chainGen(10.toByte)) { chain =>
      def customCacheHistory(cacheSize: Int): History = {
        val cacheStore = new CacheLayerKeyValueStore(new InMemoryKeyValueStore, 30.seconds, cacheSize)
        val storage = new Storage(cacheStore)
        val history = new History(storage, TineProcessor(1024))
        history.append(chain.head.block, Seq()).get._1
      }

      val history = customCacheHistory(512)
      val historyWithBlocks = chain.tail.foldLeft(history) { (accHistory, newBlock) =>
        accHistory.append(newBlock, Seq()).get._1
      }

      val cacheSize = historyWithBlocks.storage.keyValueStore
        .asInstanceOf[CacheLayerKeyValueStore]
        .cache
        .size()
      val historyInsufficientCache = customCacheHistory(cacheSize.toInt / 2)
      val historyWithTooManyBlocks = chain.tail.foldLeft(historyInsufficientCache) { (accHistory, newBlock) =>
        accHistory.append(newBlock, Seq()).get._1
      }

      historyWithBlocks.storage.keyValueStore
        .asInstanceOf[CacheLayerKeyValueStore]
        .cache
        .getIfPresent(new CacheLayerKeyValueStore.WrappedBytes(chain.tail.head.id.getIdBytes)) should not be null

      historyWithTooManyBlocks.storage.keyValueStore
        .asInstanceOf[CacheLayerKeyValueStore]
        .cache
        .getIfPresent(new CacheLayerKeyValueStore.WrappedBytes(chain.tail.head.id.getIdBytes)) shouldBe null
    }
  }

  property("blockLoader should correctly return a block from storage not found in cache") {
    forAll(chainGen(2)) { chain =>
      withTestHistory(chain.head.block) { history =>
        val updatedHistory = history.append(chain.tail.head, Seq()).get._1
        updatedHistory.storage.keyValueStore.asInstanceOf[CacheLayerKeyValueStore].cache.invalidateAll()
        updatedHistory.modifierById(chain.tail.head.id).get should not be None
      }
    }
  }

  private def chainGen: Byte => Gen[GenesisHeadChain] =
    (length: Byte) =>
      validChainFromGenesis(
        keyRingCurve25519,
        settings.application.genesis.generated.get,
        protocolVersioner
      )(length)

  private def withTestHistory(genesis: Block)(test: History => Unit): Unit =
    test {
      val underlyingStore = new InMemoryKeyValueStore
      val cacheStore = new CacheLayerKeyValueStore(underlyingStore, 10.seconds, 512)
      val storage = new Storage(cacheStore)
      val history = new History(storage, TineProcessor(1024))
      history.append(genesis, Seq()).get._1
    }
}
