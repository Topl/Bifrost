package co.topl.consensus

import cats.implicits._
import co.topl.modifier.block.Block
import co.topl.modifier.transaction.{ArbitTransfer, PolyTransfer, Transaction}
import co.topl.utils.TimeProvider
import co.topl.utils.implicits._

import scala.util.{Failure, Try}

//PoS consensus rules checks, throws exception if anything wrong
sealed trait BlockValidator[T] {
  def validate(f: Block => T)(block: Block): Try[Unit]
}

class DifficultyBlockValidator(leaderElection: NxtLeaderElection, consensusState: NxtConsensus.State)
    extends BlockValidator[(Block, Seq[TimeProvider.Time])] {

  def validate(f: Block => (Block, Seq[TimeProvider.Time]))(block: Block): Try[Unit] = Try {
    // lookup our local data about the parent
    val (parent, prevBlockTimes) = f(block)

    // first ensure that we can calculate the same block data as is stamped on the block
    ensureHeightAndDifficulty(leaderElection)(block, parent, prevBlockTimes) match {
      case Failure(ex) => throw ex
      case _           => // continue on
    }

    // next, ensure the hit was valid
    ensureValidHit(leaderElection, consensusState)(block, parent) match {
      case Failure(ex) => throw ex
      case _           => // continue on
    }
  }

  private def ensureHeightAndDifficulty(
    leaderElection: NxtLeaderElection
  )(block:          Block, parent: Block, prevTimes: Seq[TimeProvider.Time]): Try[Unit] =
    Try {
      // calculate the new base difficulty
      val newHeight = parent.height + 1
      val newBaseDifficulty =
        leaderElection.calcNewBaseDifficulty(newHeight, parent.difficulty, prevTimes)

      println(s"Local calculation difficulty, block: ${block.difficulty} expected: $newBaseDifficulty")
      println(s"Local calculation height, block: ${block.height} expected: $newHeight")

      // does the difficulty stamped on the block match what we would calculate locally?
      require(
        block.difficulty == newBaseDifficulty,
        s"Local calculation of block difficulty failed since ${block.difficulty} != $newBaseDifficulty"
      )

      // does the height stamped on the block match what we would calculate locally?
      require(
        block.height == newHeight,
        s"Local calculation of block height failed since ${block.height} != $newHeight"
      )
    }

  private def ensureValidHit(leaderElection: NxtLeaderElection, consensusState: NxtConsensus.State)(
    block:                                   Block,
    parent:                                  Block
  ): Try[Unit] = Try {
    // calculate the hit value from the forger box included in the new block
    val hit = leaderElection.calculateHitValue(parent)(block.generatorBox)

    // calculate the difficulty the forger would have used to determine eligibility
    val target = leaderElection.calculateThresholdValue(
      block.generatorBox.value.quantity,
      consensusState.totalStake,
      block.timestamp - parent.timestamp,
      parent.difficulty,
      parent.height
    )

    // did the forger create a block with a valid forger box and adjusted difficulty?
    require(BigInt(hit) < target, s"Block difficulty failed since $hit >= $target")
  }
}

class SyntaxBlockValidator(consensusState: NxtConsensus.State) extends BlockValidator[Block] {
  // todo: decide on a maximum size for blocks and enforce here

  // the signature on the block should match the signature used in the Arbit and Poly minting transactions
  val forgerEntitlementCheck: (Transaction.TX, Block) => Unit =
    (tx: Transaction.TX, b: Block) =>
      require(
        tx.attestation.keys.toSeq.contains(b.publicKey),
        "The forger entitled transactions must match the block details"
      )

  def validate(f: Block => Block)(block: Block): Try[Unit] = Try {

    // check block signature is valid
    require(block.signature.isValid(block.publicKey, block.messageToSign), "Failed to validate block signature")

    // ensure only a single Arbit minting transaction
    val numArbitCoinbase = block.transactions.count {
      case tx: ArbitTransfer[_] => tx.minting
      case _                    => false
    }
    require(numArbitCoinbase == 1, "Invalid number of Arbit reward transactions.")

    // ensure only a single Poly minting transaction
    val numPolyCoinbase = block.transactions.count {
      case tx: PolyTransfer[_] => tx.minting
      case _                   => false
    }
    require(numPolyCoinbase == 1, "Invalid number of Poly reward transactions.")

    // enforce the structure of the Arbit and Poly minting transactions
    block.transactions.zipWithIndex.map {
      case (tx, 0) =>
        tx match {
          case tx: ArbitTransfer[_] if tx.minting =>
            forgerEntitlementCheck(tx, block)
            require(
              tx.to
                .map(_._2.quantity)
                .sum == consensusState.inflation, // JAA -this needs to be done more carefully
              "The inflation amount in the block must match the output of the Arbit rewards transaction"
            )
            require(
              tx.data.fold(false)(_.show.split("_").head == block.parentId.show),
              "Arbit reward transactions must contain the parent id of their minting block"
            )

          case _ => throw new Error("The first transaction in a block must be a minting ArbitTransfer")
        }

      case (tx, 1) =>
        tx match {
          case tx: PolyTransfer[_] if tx.minting =>
            forgerEntitlementCheck(tx, block)
            require(
              block.transactions.map(_.fee).sum == tx.to.map(_._2.quantity).sum,
              "The sum of the fees in the block must match the output of the Poly rewards transaction"
            )
            require(
              tx.data.fold(false)(_.show.split("_").head == block.parentId.show),
              "Poly reward transactions must contain the parent id of their minting block"
            )

          case _ => throw new Error("The second transaction in a block must be a minting PolyTransfer")
        }

      case _ => // do nothing
    }
  }
}

class TimestampValidator extends BlockValidator[Option[TimeProvider.Time]] {

  override def validate(f: Block => Option[TimeProvider.Time])(block: Block): Try[Unit] = Try {
    f(block) match {
      case Some(parentTimestamp) =>
        require(
          block.timestamp > parentTimestamp,
          s"Block timestamp ${block.timestamp} is earlier than parent timestamp $parentTimestamp"
        )
      case None => throw new Error(s"Could not find timestamp for parent blockId=${block.parentId}")
    }
  }
}
