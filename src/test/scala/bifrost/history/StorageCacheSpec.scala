package bifrost.history


import bifrost.BifrostGenerators
import bifrost.blocks.BifrostBlock
import io.iohk.iodb.ByteArrayWrapper
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class StorageCacheSpec extends PropSpec
  with ScalaCheckPropertyChecks
  with Matchers
  with BifrostGenerators {

  var history: BifrostHistory = generateHistory

  property("The genesis block is stored in cache") {

    val genesisBlockId = ByteArrayWrapper(Array.fill(history.storage.storage.keySize)(-1: Byte))

    history.storage.blockCache.getIfPresent(genesisBlockId) shouldEqual history.storage.storage.get(genesisBlockId)
  }

  property("The new block updated is stored in cache") {

    forAll(bifrostBlockGen) { blockTemp =>

      val block:BifrostBlock = blockTemp.copy(parentId = history.bestBlockId)

      history = history.append(block).get._1

      history.storage.blockCache.getIfPresent(ByteArrayWrapper(block.id)) shouldEqual
        history.storage.storage.get(ByteArrayWrapper(block.id))
    }
  }

}
