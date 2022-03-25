package co.topl.consensus

import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.nodeView.NodeViewTestHelpers
import co.topl.utils.implicits.toEitherOps
import co.topl.utils.{CommonGenerators, TestSettings}
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec

class BlockVersionTests
    extends AnyPropSpec
    with Matchers
    with TestSettings
    with NodeViewTestHelpers
    with CommonGenerators {

  property(
    "Applying different blocks in different versions according to the test.conf should yield blocks " +
    "with intended versions"
  ) {
    val genesis = new GenesisProvider(
      protocolVersioner.applicable(1).blockVersion,
      keyRingCurve25519.addresses ++ keyRingEd25519.addresses
    )
    val genesisBlock = genesis.fetchGenesis(settings).getOrThrow().block
    val history = generateHistory(genesisBlock).history

    /**
     * Apply enough blocks to history and state so there will be blocks of all possible versions
     * Don't make a test.conf that has a version with a really large startBlock
     */
    val blocksToAppend: Long = protocolVersioner.compatibleProtocolVersions.map(_.startBlock).max
    assert(blocksToAppend <= Int.MaxValue, "Test settings incorrectly configured")

    val blocksCount: Long = blocksToAppend + 1 // with genesis block
    val listBlocks = Gen.listOfN(blocksToAppend.toInt, blockCurve25519Gen).sample.get.map { block =>
      block.copy(
        parentId = history.bestBlockId,
        transactions = Seq(),
        version = protocolVersioner.applicable(history.height + 1).blockVersion
      )
    }

    val updatedHistory = listBlocks.foldLeft(history) { case (accHistory, block) =>
      accHistory.append(block, Seq()).get._1
    }

    var currentId: ModifierId = updatedHistory.bestBlockId
    for (height <- blocksCount to 1 by -1) {
      val currentBlock: Block = updatedHistory.modifierById(currentId).get
      currentId = currentBlock.parentId
      log.debug(s"${Console.MAGENTA}$currentBlock${Console.RESET}")
      val versionConf = protocolVersioner.applicable(height).blockVersion
      versionConf == currentBlock.version shouldBe true
    }
  }

  property("Applying genesis block to history/state with different available versions should be successful") {
    for (version <- protocolVersioner.compatibleProtocolVersions.map(_.blockVersion)) {
      val genesis = new GenesisProvider(version, keyRingCurve25519.addresses ++ keyRingEd25519.addresses)
      val genesisBlock = genesis.fetchGenesis(settings).getOrThrow().block

      val history = generateHistory(genesisBlock).history
      val state = generateState(genesisBlock).state

      history.modifierById(genesisBlock.id).isDefined shouldBe true
      state.version == genesisBlock.id shouldBe true
    }
  }
}
