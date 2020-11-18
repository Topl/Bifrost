package co.topl.consensus

import co.topl.modifier.block.Block
import co.topl.modifier.transaction.{ArbitTransfer, PolyTransfer, Transaction}
import co.topl.nodeView.history.{BlockProcessor, Storage}

import scala.util.Try

//PoS consensus rules checks, throws exception if anything wrong
trait BlockValidator[PM <: Block] {
  def validate(block: PM): Try[Unit]
}

class DifficultyBlockValidator(storage: Storage, blockProcessor: BlockProcessor) extends BlockValidator[Block] {
  def validate(block: Block): Try[Unit] = Try {
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

class SyntaxBlockValidator extends BlockValidator[Block] {
  // the signature on the block should match the signature used in the Arbit and Poly minting transactions
  val forgerEntitlementCheck: (Transaction[_,_], Block) => Unit =
    (tx: Transaction[_,_], b: Block) =>
      require(tx.attestation.keys.toSeq.contains(b.publicKey),
        "The forger entitled transactions must match the block details")

  def validate(block: Block): Try[Unit] = Try {
    // check block signature is valid
    require(block.signature.isValid(block.publicKey, block.messageToSign), "Failed to validate block signature")


    // ensure only a single Arbit minting transaction
    val numArbitCoinbase = block.transactions.count {
      case tx: ArbitTransfer[_] => tx.minting
    }
    require(numArbitCoinbase == 1, "Invalid number of Arbit rewards transactions.")

    // ensure only a single Poly minting transaction
    val numPolyCoinbase = block.transactions.count {
      case tx: PolyTransfer[_] => tx.minting
    }
    require(numPolyCoinbase == 1, "Invalid number of Poly rewards transactions.")

    // enforce the structure of the Arbit and Poly minting transactions
    block.transactions.zipWithIndex.map {
      case (tx, 0) => tx match {
        case tx: ArbitTransfer[_] if tx.minting =>
          forgerEntitlementCheck(tx, block)
          require(tx.to.map(_._2).sum == inflation,
                  "The inflation amount in the block must match the output of the Arbit rewards transaction")

        case _ => throw new Error("The first transaction in a block must be a minting ArbitTransfer")
      }

      case (tx, 1) => tx match {
        case tx: PolyTransfer[_] if tx.minting =>
          forgerEntitlementCheck(tx, block)
          require(block.transactions.map(_.fee).sum == tx.to.map(_._2).sum,
                  "The sum of the fees in the block must match the output of the Poly rewards transaction")

        case _ => throw new Error("The second transaction in a block must be a minting PolyTransfer")
      }
    }
  }
}
