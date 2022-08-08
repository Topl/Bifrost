package co.topl.consensus

import cats.data.NonEmptyChain
import co.topl.modifier.block.Block
import co.topl.nodeView.NodeViewTestHelpers
import co.topl.utils.implicits.toEitherOps
import co.topl.utils.{CommonGenerators, TestSettings}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec

class BlockVersionTests
    extends AnyPropSpec
    with Matchers
    with TestSettings
    with ValidBlockchainGenerator
    with NodeViewTestHelpers
    with CommonGenerators {

  property(
    "Applying different blocks in different versions according to the test.conf should yield blocks " +
    "with intended versions"
  ) {

    /**
     * Apply enough blocks to history and state so there will be blocks of all possible versions
     * Don't make a test.conf that has a version with a really large startBlock
     */
    val blocksToAppend: Long = protocolVersioner.compatibleProtocolVersions.map(_.startBlock).max
    assert(blocksToAppend <= Byte.MaxValue, "Test settings incorrectly configured")

    // val blocksCount: Long = blocksToAppend + 1 // with genesis block
    val listBlocks: NonEmptyChain[Block] =
      validChainFromGenesis(
        keyRingCurve25519,
        settings.application.genesis.generated.get,
        protocolVersioner
      )(blocksToAppend.toByte).sample.get.tail

//    val history = generateHistory(listBlocks.head).history
//    val updatedHistory = listBlocks.tail.foldLeft(history) { case (accHistory, block) =>
//      accHistory.append(block, Seq()).get._1
//    }

    listBlocks.zipWithIndex.forall { case (block, height) =>
      block.version == protocolVersioner.applicable(height).blockVersion
    }
//    var currentId: ModifierId = updatedHistory.bestBlockId
//    for (height <- blocksCount to 1 by -1) {
//      val currentBlock: Block = updatedHistory.modifierById(currentId).get
//      currentId = currentBlock.parentId
//      log.debug(s"${Console.MAGENTA}$currentBlock${Console.RESET}")
//      val versionConf = protocolVersioner.applicable(height).blockVersion
//      versionConf == currentBlock.version shouldBe true
//    }
  }

  property("Applying genesis block to history/state with different available versions should be successful") {
    for (version <- protocolVersioner.compatibleProtocolVersions.map(_.blockVersion)) {
      val genesis = new GenesisProvider(version, keyRingCurve25519.addresses ++ keyRingEd25519.addresses)
        .fetchGenesis(settings)
        .getOrThrow()

      val history = generateHistory(genesis).history
      val state = generateState(genesis.block).state

      history.modifierById(genesis.block.id).isDefined shouldBe true
      state.version == genesis.block.id shouldBe true
    }
  }
}
