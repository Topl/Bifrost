package co.topl.consensus

import co.topl.modifier.block.{ Block, BlockValidator }
import co.topl.nodeView.history.{ BlockProcessor, Storage }
import co.topl.modifier.block.Block

import scala.util.Try

class DifficultyBlockValidator(storage: Storage, blockProcessor: BlockProcessor) extends BlockValidator[Block] {

  def validate(block: Block): Try[Unit] = checkConsensusRules(block)

  //PoS consensus rules checks, throws exception if anything wrong
  private def checkConsensusRules(block: Block): Try[Unit] = Try {
    if (!storage.isGenesis(block)) {
      // find the source of the parent block (either storage or chain cache)
      val (parent, parentDifficulty) = blockProcessor.getCacheBlock(block.parentId) match {
        case Some(cacheParent) =>
          (cacheParent.block, cacheParent.baseDifficulty)
        case None =>
          (storage.modifierById(block.parentId).get, storage.parentDifficulty(block))
      }

      // calculate the hit value from the forger box included in the new block
      val hit = calcHit(parent)(block.forgerBox)

      // calculate the adjusted difficulty the forger would have used to determine eligibility
      val timestamp = block.timestamp
      val target = calcAdjustedTarget(parent, parentDifficulty, timestamp)
      val valueTarget = (target * BigDecimal(block.forgerBox.value)).toBigInt

      // did the forger create a block with a valid forger box and adjusted difficulty?
      require( BigInt(hit) < valueTarget, s"$hit < $valueTarget failed, $parentDifficulty ")
    }
  }
}
