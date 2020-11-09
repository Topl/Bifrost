package co.topl.consensus

import co.topl.modifier.block.Block
import co.topl.modifier.transaction.{ArbitTransfer, PolyTransfer}
import co.topl.nodeView.history.{BlockProcessor, Storage}

import scala.util.Try

//PoS consensus rules checks, throws exception if anything wrong
trait BlockValidator[PM <: Block] {
  def validate(block: PM): Try[Unit]
}

class DifficultyBlockValidator(storage: Storage, blockProcessor: BlockProcessor) extends BlockValidator[Block] {
  def validate(block: Block): Try[Unit] = Try {
    if (!storage.isGenesis(block)) {
      // find the source of the parent block (either storage or chain cache)
      val (parent, parentDifficulty, parentHeight) = blockProcessor.getCacheBlock(block.parentId) match {
        case Some(cacheParent) =>
          (cacheParent.block, cacheParent.baseDifficulty, cacheParent.height)
        case None =>
          (storage.modifierById(block.parentId).get,
            storage.difficultyOf(block.parentId).get,
            storage.heightOf(block.parentId).get)
      }

      // calculate the hit value from the forger box included in the new block
      val hit = calcHit(parent)(block.forgerBox)

      // calculate the adjusted difficulty the forger would have used to determine eligibility
      val timestamp = block.timestamp
      val target = calcAdjustedTarget(parent, parentHeight, parentDifficulty, timestamp)
      val valueTarget = (target * BigDecimal(block.forgerBox.value)).toBigInt

      // did the forger create a block with a valid forger box and adjusted difficulty?
      require( BigInt(hit) < valueTarget, s"$hit < $valueTarget failed, $parentDifficulty ")
    }
  }
}

class SyntaxBlockValidator extends BlockValidator[Block] {
  def validate(block: Block): Try[Unit] = Try {
    // check block signature is valid
    require(block.signature.isValid(block.publicKey, block.messageToSign), "Failed to validate block signature")

    // ensure only a single Arbit coinbase transaction
    val numArbitCoinbase = block.transactions.count{
      case tx: ArbitTransfer[_,_] => tx.minting
    }
    require(numArbitCoinbase == 1, "Invalid number of Arbit coinbase transactions.")

    // ensure only a single Poly coinbase transaction (as of 2020.11.08 this is zero)
    val numPolyCoinbase = block.transactions.count{
      case tx: PolyTransfer[_,_] => tx.minting
    }
    require(numPolyCoinbase == 0, "Invalid number of Poly coinbase transactions.")
  }
}
