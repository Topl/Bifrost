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
import co.topl.settings.ProtocolConfigurations
import co.topl.utils.{Int128, TimeProvider}
import com.google.common.primitives.Longs

import scala.collection.Set
import scala.concurrent.duration.MILLISECONDS
import scala.math.{max, min}

class NxtLeaderElection(protocolVersioner: ProtocolVersioner) {

  /**
   * Defines how we calculate the test value for determining eligibility to forge
   *
   * @param lastBlock previous block
   * @param box       box to be used for the test value
   * @return the test value to be compared to the adjusted difficulty
   */
  private[consensus] def calcHit(lastBlock: Block)(box: ArbitBox): Long = {
    val h = blake2b256.hash(
      // need to use Persistable instances of parent types
      Persistable[NodeViewModifier].persistedBytes(lastBlock) ++
      Persistable[Box[_]].persistedBytes(box)
    )

    Longs.fromByteArray((0: Byte) +: h.value.take(7))
  }

  /**
   * Gets the target threshold.
   * threshold = ( (address stake) * (time delta) * (difficulty) ) / ( (total stake) * (target block time) )
   *
   * @param stakeAmount  amount of stake held in address
   * @param totalStake   amount of
   * @param timeDelta    delta from previous block time to the current time
   * @param difficulty   forging difficulty
   * @param parentHeight parent block height
   * @return the target value
   */
  private[consensus] def calcTarget(
    stakeAmount:  Int128,
    totalStake:   Int128,
    timeDelta:    Long,
    difficulty:   Long,
    parentHeight: Long
  ): BigInt = {
    val targetBlockTime = NxtLeaderElection.getNxtProtocolConfigs(protocolVersioner, parentHeight).targetBlockTime

    (BigInt(stakeAmount.toByteArray) * BigInt(timeDelta) * BigInt(difficulty)) /
    (BigInt(totalStake.toByteArray) * BigInt(targetBlockTime.toUnit(MILLISECONDS).toLong))
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
  def calcNewBaseDifficulty(newHeight: Long, prevDifficulty: Long, prevTimes: Seq[TimeProvider.Time]): Long = {

    val averageDelay = prevTimes.drop(1).lazyZip(prevTimes).map(_ - _).sum / (prevTimes.length - 1)
    val targetTimeMilli = NxtLeaderElection
      .getNxtProtocolConfigs(protocolVersioner, newHeight)
      .targetBlockTime
      .toUnit(MILLISECONDS)

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

  /**
   * Returns Dion specific protocol configs that can be handled by the current (1.10.2) application version
   * @param protocolVersioner class responsible for abstractly managing the protocol versions
   * @param height height of the chain,
   * @return
   */
  def getNxtProtocolConfigs(protocolVersioner: ProtocolVersioner, height: Long): ProtocolConfigurations.Dion =
    protocolVersioner.applicable(height).value

  /**
   * Gets an arbit box that is eligible for forging the next block if there are any.
   * @param parent the parent block
   * @param addresses the addresses to stake with
   * @param timestamp the current time
   * @param stateReader a read-only version of state
   * @return an eligible box if one is found
   */
  def getEligibleBox(
    parent:            Block,
    addresses:         Set[Address],
    timestamp:         TimeProvider.Time,
    consensusState:    NxtConsensus.State,
    nxtLeaderElection: NxtLeaderElection,
    stateReader:       SR
  ): Either[IneligibilityReason, ArbitBox] =
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

      if (arbitBoxesIterator.hasNext) {
        while (arbitBoxesIterator.hasNext) {
          val box = arbitBoxesIterator.next()
          val hit = nxtLeaderElection.calcHit(parent)(box)
          val calculatedTarget =
            nxtLeaderElection.calcTarget(
              box.value.quantity,
              consensusState.totalStake,
              timestamp - parent.timestamp,
              parent.difficulty,
              parent.height
            )
          if (BigInt(hit) < calculatedTarget) return Right(box)
        }
        Left(NoBoxesEligible)
      } else {
        Left(NoArbitBoxesAvailable)
      }
    }

  sealed trait IneligibilityReason
  case object NoAddressesAvailable extends IneligibilityReason
  case object NoBoxesEligible extends IneligibilityReason
  case object NoArbitBoxesAvailable extends IneligibilityReason
}
