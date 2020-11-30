package co.topl.consensus

import co.topl.consensus
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.modifier.transaction.{ArbitTransfer, PolyTransfer, Transaction}
import co.topl.nodeView.history.{BlockProcessor, Storage}

import scala.annotation.tailrec
import scala.util.{Failure, Try}

//PoS consensus rules checks, throws exception if anything wrong
sealed trait BlockValidator[PM <: Block] {
  def validate(block: PM): Try[Unit]
}

class DifficultyBlockValidator(storage: Storage, blockProcessor: BlockProcessor) extends BlockValidator[Block] {
  def validate(block: Block): Try[Unit] = Try {

    // first ensure that we can calculate the same block data as is stamped on the block
    ensureHeightAndDifficulty(storage, block) match {
      case Failure(ex) => throw ex
      case _ => // continue on
    }

    // next, ensure the hit was valid
    val (parent, parentDifficulty, parentHeight) = getParentDetails(block.parentId)
    checkValidHit(block, parent, parentHeight, parentDifficulty) match {
      case Failure(ex) => throw ex
      case _ => // continue on
    }
  }

  /** Helper function to find the source of the parent block (either storage or chain cache) */
  //todo: JAA - no longer need to separately pull the difficulty and height since they are in the block,
  //            can also remove these fields from cache blocks
  private def getParentDetails(parentId: ModifierId): (Block, Long, Long) =
    blockProcessor.getCacheBlock(parentId) match {
      case Some(cacheParent) =>
        (cacheParent.block, cacheParent.baseDifficulty, cacheParent.height)
      case None =>
        (
          storage.modifierById(parentId).get, //we have already checked if the parent exists so can get
          storage.difficultyAt(parentId),
          storage.heightAt(parentId)
        )
    }

  /** Gets the timestamps for 'count' number of blocks prior to the startBlock */
  def getTimestamps(storage: Storage, count: Long, startBlock: Block): Seq[Block.Timestamp] = {
    @tailrec
    def loop(b: Block, acc: Seq[Block.Timestamp] = Seq()): Seq[Block.Timestamp] = {
      if (acc.length >= count) acc
      else storage.modifierById(b.parentId) match {
        case Some(parent: Block) => loop(parent, b.timestamp +: acc)
        case _ => b.timestamp +: acc
      }
    }

    loop(startBlock)
  }

  private def ensureHeightAndDifficulty(storage: Storage, block: Block): Try[Unit] = Try {
    // calculate the new base difficulty
    val parentDifficulty = storage.difficultyOf(block.parentId).get
    val prevTimes = getTimestamps(storage, consensus.nxtBlockNum + 1, block)
    val newHeight = storage.heightOf(block.parentId).get + 1
    val newBaseDifficulty = consensus.calcNewBaseDifficulty(newHeight, parentDifficulty, prevTimes)

    // does the difficulty stamped on the block match what we would calculate locally?
    require(block.difficulty == newBaseDifficulty,
      s"Local calculation of block difficulty failed since ${block.difficulty} != $newBaseDifficulty")

    // does the height stamped on the block match what we would calculate locally?
    require(block.height == newHeight,
      s"Local calculation of block height failed since ${block.height} != $newHeight")
  }

  private def checkValidHit(block: Block, parent: Block, parentHeight: Long, parentDifficulty: Long): Try[Unit] = Try {
    // calculate the hit value from the forger box included in the new block
    val hit = calcHit(parent)(block.generatorBox)

    // calculate the adjusted difficulty the forger would have used to determine eligibility
    val timestamp = block.timestamp
    val target = calcAdjustedTarget(parent, parentHeight, parentDifficulty, timestamp)
    val valueTarget = (target * BigDecimal(block.generatorBox.value)).toBigInt

    // did the forger create a block with a valid forger box and adjusted difficulty?
    require(BigInt(hit) < valueTarget, s"Block difficulty failed since $hit > $valueTarget")
  }
}

/* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- */

class SyntaxBlockValidator extends BlockValidator[Block] {
  //todo: decide on a maximum size for blocks and enforce here

  // the signature on the block should match the signature used in the Arbit and Poly minting transactions
  val forgerEntitlementCheck: (Transaction.TX, Block) => Unit =
    (tx: Transaction.TX, b: Block) =>
      require(tx.attestation.keys.toSeq.contains(b.publicKey),
        "The forger entitled transactions must match the block details")

  def validate(block: Block): Try[Unit] = Try {

    // check block signature is valid
    require(block.signature.isValid(block.publicKey, block.messageToSign), "Failed to validate block signature")

    // ensure only a single Arbit minting transaction
    val numArbitCoinbase = block.transactions.count {
      case tx: ArbitTransfer[_] => tx.minting
      case _ => false
    }
    require(numArbitCoinbase == 1, "Invalid number of Arbit reward transactions.")

    // ensure only a single Poly minting transaction
    val numPolyCoinbase = block.transactions.count {
      case tx: PolyTransfer[_] => tx.minting
      case _ => false
    }
    require(numPolyCoinbase == 1, "Invalid number of Poly reward transactions.")

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

      case _ => // do nothing
    }
  }
}
