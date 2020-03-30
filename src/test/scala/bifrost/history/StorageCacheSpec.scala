package bifrost.history


import bifrost.BifrostGenerators
import io.iohk.iodb.ByteArrayWrapper
import org.scalatest.{Matchers, PropSpec}

class StorageCacheSpec extends PropSpec
  with Matchers
  with BifrostGenerators {

  val history: BifrostHistory = generateHistory

  property("The genesis block is stored in cache") {

    val genesisBlockId = ByteArrayWrapper(Array.fill(history.storage.storage.keySize)(-1: Byte))

    history.storage.blockCache.getIfPresent(genesisBlockId) shouldEqual history.storage.storage.get(genesisBlockId)
  }
}
