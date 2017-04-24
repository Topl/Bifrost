package examples.bifrost.validation

import examples.bifrost.blocks.BifrostBlock
import examples.bifrost.forging.Forger
import examples.bifrost.history.BifrostStorage
import scorex.core.block.BlockValidator
import scorex.crypto.encode.Base58

import scala.util.Try

class DifficultyBlockValidator(storage: BifrostStorage)
  extends BlockValidator[BifrostBlock] {

  def validate(block: BifrostBlock): Try[Unit] = checkConsensusRules(block)

  //PoS consensus rules checks, throws exception if anything wrong
  private def checkConsensusRules(block: BifrostBlock): Try[Unit] = Try {
    if (!storage.isGenesis(block)) {
      val lastBlock = storage.modifierById(block.parentId).get
      println(s"LastBlock Id is ${Base58.encode(lastBlock.id)}")
      println("")
      val hit = Forger.hit(lastBlock)(block.generatorBox)
      val difficulty = storage.difficultyOf(block.parentId).get
      // val target = (Forger.MaxTarget / difficulty) * block.generatorBox.value
      val target = Forger.calcAdjustedTarget(difficulty, lastBlock, storage.settings.blockGenerationDelay.length)
      require(BigInt(hit) < target * BigInt(block.generatorBox.value), s"$hit < $target failed, $difficulty, ")
    }

  }

}
