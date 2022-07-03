package co.topl.consensus

import cats.implicits._
import co.topl.modifier.block.Block
import co.topl.modifier.transaction.{ArbitTransfer, PolyTransfer, Transaction}
import co.topl.utils.TimeProvider
import co.topl.utils.implicits._

import scala.util.Try

//PoS consensus rules checks, throws exception if anything wrong
sealed trait BlockValidator[T] {
  def validate(f: Block => T)(block: Block): Try[Unit]
}

object BlockValidators {

  class HeightValidator extends BlockValidator[Option[Long]] {

    override def validate(f: Block => Option[Long])(block: Block): Try[Unit] = Try {
      f(block) match {
        case Some(parentHeight) =>
          require(
            block.height == parentHeight + 1,
            s"Invalid child block height from parent; ${block.height} != ${parentHeight + 1}"
          )
        case None => throw new Error(s"Could not find parent height for blockId=${block.parentId}")
      }
    }
  }

  class DifficultyValidator(leaderElection: NxtLeaderElection)
      extends BlockValidator[Option[(Block, Seq[TimeProvider.Time])]] {

    override def validate(f: Block => Option[(Block, Seq[TimeProvider.Time])])(block: Block): Try[Unit] = Try {
      f(block) match {
        case Some((parent, prevTimes)) =>
          val childBaseDifficulty =
            leaderElection.calculateNewDifficulty(parent.height + 1, parent.difficulty, prevTimes)
          require(
            block.difficulty == childBaseDifficulty,
            s"Local calculation of block difficulty failed since ${block.difficulty} != $childBaseDifficulty"
          )

        case None => throw new Error(s"Could not find parent with blockId=${block.parentId}")
      }
    }
  }

  class EligibilityValidator(leaderElection: NxtLeaderElection, consensusState: NxtConsensus.State)
      extends BlockValidator[Option[Block]] {

    override def validate(fetchParentOf: Block => Option[Block])(block: Block): Try[Unit] = Try {
      fetchParentOf(block) match {
        case Some(parent) =>
          val timeSinceLastBlack = block.timestamp - parent.timestamp
          val hit = leaderElection.calculateHitValue(parent)(_)
          val threshold = leaderElection.calculateThresholdValue(timeSinceLastBlack, consensusState)(_)

          // did the forger create a block with a valid forger box and adjusted difficulty?
          require(
            NxtLeaderElection.getEligibleBox(hit, threshold)(Iterator(block.generatorBox)).isRight,
            s"Failed to verify eligibility for blockId=${block.id}" +
            s"since ${hit(block.generatorBox)} >= ${threshold(block.generatorBox)}"
          )

        case None => throw new Error(s"Could not find parent with blockId=${block.parentId}")
      }
    }
  }

  class SyntaxValidator(consensusState: NxtConsensus.State) extends BlockValidator[Block] {
    // todo: decide on a maximum size for blocks and enforce here

    // the signature on the block should match the signature used in the Arbit and Poly minting transactions
    private val forgerEntitlementCheck: (Transaction.TX, Block) => Unit =
      (tx: Transaction.TX, b: Block) =>
        require(
          tx.attestation.keys.toSeq.contains(b.publicKey),
          "The forger entitled transactions must match the block details"
        )

    override def validate(f: Block => Block)(block: Block): Try[Unit] = Try {

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

}
