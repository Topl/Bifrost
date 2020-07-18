package bifrost.consensus

import bifrost.history.Storage
import bifrost.modifier.block.{Block, BlockValidator}

import scala.util.Try

class DifficultyBlockValidator(storage: Storage) extends BlockValidator[Block] {

  def validate(block: Block): Try[Unit] = checkConsensusRules(block)

  //PoS consensus rules checks, throws exception if anything wrong
  private def checkConsensusRules(block: Block): Try[Unit] = Try {
    if (!storage.isGenesis(block)) {
      val lastBlock = storage.modifierById(block.parentId).get
      val hit = calcHit(lastBlock)(block.forgerBox)
      val difficulty = storage.difficultyOf(block.parentId).get
      val target = calcAdjustedTarget(difficulty, lastBlock, storage.settings.forgingSettings.targetBlockTime)

      require( BigInt(hit) < target * BigInt(block.forgerBox.value), s"$hit < $target failed, $difficulty, ")
    }
  }
}
