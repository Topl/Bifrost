package bifrost.consensus

import bifrost.history.Storage
import bifrost.modifier.block.{Block, BlockValidator}

import scala.util.Try

class DifficultyBlockValidator(storage: Storage) extends BlockValidator[Block] {

  def validate(block: Block): Try[Unit] = checkConsensusRules(block)

  //PoS consensus rules checks, throws exception if anything wrong
  private def checkConsensusRules(block: Block): Try[Unit] = Try {
    if (!storage.isGenesis(block)) {
      val parent = storage.modifierById(block.parentId).get
      val parentDifficulty = storage.parentDifficulty(block)
      val targetTime = storage.settings.forgingSettings.targetBlockTime
      val timestamp = block.timestamp

      val hit = calcHit(parent)(block.forgerBox)
      val target = calcAdjustedTarget(parent, parentDifficulty, targetTime, timestamp)

      require( BigInt(hit) < (target * BigInt(block.forgerBox.value)), s"$hit < $target failed, $parentDifficulty, ")
    }
  }
}
