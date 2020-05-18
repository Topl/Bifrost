package bifrost.consensus

import bifrost.modifier.block.{Block, BlockValidator}
import bifrost.forging.Forger
import bifrost.history.BifrostStorage

import scala.util.Try

class DifficultyBlockValidator(storage: BifrostStorage) extends BlockValidator[Block] {

  def validate(block: Block): Try[Unit] = checkConsensusRules(block)

  //PoS consensus rules checks, throws exception if anything wrong
  private def checkConsensusRules(block: Block): Try[Unit] = Try {
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
