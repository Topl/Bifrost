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
      val timestamp = block.timestamp

      val hit = calcHit(parent)(block.forgerBox)
      val target = calcAdjustedTarget(parent, parentDifficulty, timestamp)
      val valueTarget = (target * BigDecimal(block.forgerBox.value)).toBigInt

      require( BigInt(hit) < valueTarget, s"$hit < $valueTarget failed, $parentDifficulty, ")
    }
  }
}
