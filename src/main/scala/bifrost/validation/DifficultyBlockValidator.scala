package bifrost.validation

import bifrost.blocks.BifrostBlock
import bifrost.forging.Forger
import bifrost.history.BifrostStorage
import bifrost.block.BlockValidator

import scala.util.Try

class DifficultyBlockValidator(storage: BifrostStorage) extends BlockValidator[BifrostBlock] {

  def validate(block: BifrostBlock): Try[Unit] = checkConsensusRules(block)

  //PoS consensus rules checks, throws exception if anything wrong
  private def checkConsensusRules(block: BifrostBlock): Try[Unit] = Try {
    if (!storage.isGenesis(block)) {
      val lastBlock = storage.modifierById(block.parentId).get
      val hit = Forger.hit(lastBlock)(block.forgerBox)
      val difficulty = storage.difficultyOf(block.parentId).get
      // val target = (Forger.MaxTarget / difficulty) * block.generatorBox.value
      val target = Forger.calcAdjustedTarget(difficulty, lastBlock, storage.settings.targetBlockTime.length)
      require(BigInt(hit) < target * BigInt(block.forgerBox.value), s"$hit < $target failed, $difficulty, ")
    }
  }
}
