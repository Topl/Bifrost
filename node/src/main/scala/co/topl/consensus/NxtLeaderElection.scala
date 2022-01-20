package co.topl.consensus

import co.topl.crypto.hash.blake2b256
import co.topl.modifier.block.Block
import co.topl.modifier.box.ArbitBox
import co.topl.settings.AppSettings
import co.topl.utils.{Int128, TimeProvider}
import com.google.common.primitives.Longs

import scala.concurrent.duration.MILLISECONDS
import scala.math.{max, min}

class NxtLeaderElection(val protocolMngr: ProtocolVersioner) {
  // number of blocks to use for determining the avg block delay
  def nxtBlockNum: Int = 3

  /**
   * Defines how we calculate the test value for determining eligibility to forge
   *
   * @param lastBlock previous block
   * @param box       box to be used for the test value
   * @return the test value to be compared to the adjusted difficulty
   */
  def calcHit(lastBlock: Block)(box: ArbitBox): Long = {
    val h = blake2b256.hash(lastBlock.bytes ++ box.bytes)

    Longs.fromByteArray((0: Byte) +: h.value.take(7))
  }

  /**
   * Gets the target threshold.
   * threshold = ( (address stake) * (time delta) * (difficulty) ) / ( (total stake) * (target block time) )
   * @param stakeAmount amount of stake held in address
   * @param totalStake amount of
   * @param timeDelta delta from previous block time to the current time
   * @param difficulty forging difficulty
   * @param parentHeight parent block height
   * @return the target value
   */
  def calcTarget(
    stakeAmount:  Int128,
    totalStake:   Int128,
    timeDelta:    Long,
    difficulty:   Long,
    parentHeight: Long
  ): BigInt =
    (BigInt(stakeAmount.toByteArray) * BigInt(difficulty) * BigInt(timeDelta)) /
    (BigInt(totalStake.toByteArray) * BigInt(
      protocolMngr.targetBlockTime(parentHeight).toUnit(MILLISECONDS).toLong
    ))

  /**
   * Calculate the block difficulty according to
   * [[https://nxtdocs.jelurida.com/Nxt_Whitepaper#Block_Creation_.28Forging.29]]
   *
   * @param prevDifficulty the previous base difficulty
   * @param prevTimes      sequence of block times to calculate the average and compare to target
   * @return the modified difficulty
   */
  def calcNewBaseDifficulty(newHeight: Long, prevDifficulty: Long, prevTimes: Seq[TimeProvider.Time]): Long = {

    val averageDelay = prevTimes.drop(1).lazyZip(prevTimes).map(_ - _).sum / (prevTimes.length - 1)
    val targetTimeMilli = protocolMngr.targetBlockTime(newHeight).toUnit(MILLISECONDS)

    // magic numbers here (1.1, 0.9, and 0.64) are straight from NXT
    if (averageDelay > targetTimeMilli) {
      (prevDifficulty * min(averageDelay.toDouble, targetTimeMilli * 1.1) / targetTimeMilli).toLong
    } else {
      (prevDifficulty * (1 - 0.64 * (1 - (max(averageDelay.toDouble, targetTimeMilli * 0.9) / targetTimeMilli)))).toLong
    }
  }
}

object NxtLeaderElection {

  def apply(settings: AppSettings): NxtLeaderElection = {
    val protocolMngr = ProtocolVersioner(settings.application.version, settings.forging.protocolVersions)
    new NxtLeaderElection(protocolMngr)
  }
}
