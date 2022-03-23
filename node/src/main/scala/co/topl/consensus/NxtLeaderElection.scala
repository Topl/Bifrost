package co.topl.consensus

import co.topl.attestation.Address
import co.topl.codecs._
import co.topl.codecs.binary.typeclasses.Persistable
import co.topl.crypto.hash.blake2b256
import co.topl.modifier.NodeViewModifier
import co.topl.modifier.block.Block
import co.topl.modifier.box.{ArbitBox, Box, ProgramId}
import co.topl.modifier.transaction.Transaction
import co.topl.nodeView.state.StateReader
import co.topl.utils.TimeProvider

import scala.collection.Set
import scala.math.{max, min}

class NxtLeaderElection(protocolVersioner: ProtocolVersioner) {

  /**
   * Defines how we calculate the test value for determining eligibility to forge
   *
   * @param lastBlock previous block
   * @param box       box to be used for the test value
   * @return the test value to be compared to the adjusted difficulty
   */
  private[consensus] def calculateHitValue(lastBlock: Block)(box: ArbitBox): BigInt = {
    val h = blake2b256.hash(
      // need to use Persistable instances of parent types
      Persistable[NodeViewModifier].persistedBytes(lastBlock) ++
      Persistable[Box[_]].persistedBytes(box)
    )

    BigInt((0: Byte) +: h.value.take(7))
  }

  /**
   * Gets the target threshold.
   * threshold = ( (address stake) * (time delta) * (difficulty) ) / ( (total stake) * (target block time) )
   *
   * Another way to view this equation is to multiply two numbers between 0 and 1 and scale that product by the difficulty
   *              (box stake)      (time delta)
   * threshold = -------------  x  -------------  x  (difficulty)
   *             (total stake)     (target time)
   *
   * @param stakeAmount  amount of stake held in address
   * @param totalStake   amount of
   * @param timeDelta    delta from previous block time to the current time
   * @param difficulty   forging difficulty, this is a scale factor for determining the threshold. Unfortunately named as difficulty going makes it is easier to produce a block
   * @param parentHeight parent block height
   * @return the target value
   */
  private[consensus] def calculateThresholdValue(timeDelta: Long, consensusState: NxtConsensus.State)(
    box:                                                    ArbitBox
  ): BigInt = {
    val targetBlockTime = protocolVersioner.applicable(consensusState.height + 1).value.targetBlockTime

    (BigInt(box.value.quantity.toByteArray) * BigInt(timeDelta) * BigInt(consensusState.difficulty)) /
    (BigInt(consensusState.totalStake.toByteArray) * BigInt(targetBlockTime.toMillis))
  }

  /**
   * Calculate the block difficulty according to
   * [[https://nxtdocs.jelurida.com/Nxt_Whitepaper#Block_Creation_.28Forging.29]]
   *
   * @param prevDifficulty the previous base difficulty
   * @param prevTimes      sequence of block times to calculate the average and compare to target
   * @return the modified difficulty
   */

  // used in a node view test, so made public for now
  def calculateNewDifficulty(newHeight: Long, prevDifficulty: Long, prevTimes: Seq[TimeProvider.Time]): Long = {
    val averageDelay = prevTimes.drop(1).lazyZip(prevTimes).map(_ - _).sum / (prevTimes.length - 1)
    val targetTimeMilli = protocolVersioner
      .applicable(newHeight)
      .value
      .targetBlockTime
      .toMillis

    // magic numbers here (1.1, 0.9, and 0.64) are straight from NXT
    if (averageDelay > targetTimeMilli) {
      (prevDifficulty * min(averageDelay.toDouble, targetTimeMilli * 1.1) / targetTimeMilli).toLong
    } else {
      (prevDifficulty * (1 - 0.64 * (1 - (max(averageDelay.toDouble, targetTimeMilli * 0.9) / targetTimeMilli)))).toLong
    }
  }
}

object NxtLeaderElection {
  type TX = Transaction.TX
  type SR = StateReader[ProgramId, Address]

  def collectArbitBoxes(
    addresses:   Set[Address],
    stateReader: SR
  ): Either[IneligibilityReason, Iterator[ArbitBox]] =
    if (addresses.isEmpty) {
      Left(NoAddressesAvailable)
    } else {
      // This is ugly iterable/procedural code, but the goal is lazy traversal to avoid fetching all boxes for
      // all addresses when we're only looking for the _first_ valid candidate
      val arbitBoxesIterator =
        addresses.iterator
          .flatMap {
            stateReader
              .getTokenBoxes(_)
              .getOrElse(Nil)
          }
          .collect { case box: ArbitBox => box }

      Right(arbitBoxesIterator)
    }

  /**
   * Gets an arbit box that is eligible for forging the next block if there are any.
   * @param parent the parent block
   * @param addresses the addresses to stake with
   * @param timestamp the current time
   * @param stateReader a read-only version of state
   * @return an eligible box if one is found
   */
  def getEligibleBox(
    hitForBox:          ArbitBox => BigInt,
    thresholdForBox:    ArbitBox => BigInt
  )(arbitBoxesIterator: Iterator[ArbitBox]): Either[IneligibilityReason, ArbitBox] =
    // This is ugly iterable/procedural code, but the goal is lazy traversal to avoid fetching all boxes for
    // all addresses when we're only looking for the _first_ valid candidate
    if (arbitBoxesIterator.hasNext) {
      while (arbitBoxesIterator.hasNext) {
        val box = arbitBoxesIterator.next()
        if (hitForBox(box) < thresholdForBox(box)) return Right(box)
      }
      Left(NoBoxesEligible)
    } else {
      Left(NoArbitBoxesAvailable)
    }

  sealed trait IneligibilityReason
  case object NoAddressesAvailable extends IneligibilityReason
  case object NoBoxesEligible extends IneligibilityReason
  case object NoArbitBoxesAvailable extends IneligibilityReason
}
