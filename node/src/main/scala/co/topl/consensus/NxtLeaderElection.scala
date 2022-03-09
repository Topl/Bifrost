package co.topl.consensus

import co.topl.codecs._
import co.topl.codecs.binary.typeclasses.Persistable
import co.topl.crypto.hash.blake2b256
import co.topl.modifier.NodeViewModifier
import co.topl.modifier.block.Block
import co.topl.modifier.box.{ArbitBox, Box}
import co.topl.settings.AppSettings
import co.topl.utils.{Int128, TimeProvider}
import com.google.common.primitives.Longs

import scala.concurrent.duration.MILLISECONDS
import scala.math.{max, min}

class NxtLeaderElection(private val supportedProtocolVersions: ProtocolVersioner) {
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
                                     stakeAmount: Int128,
                                     totalStake: Int128,
                                     timeDelta: Long,
                                     difficulty: Long,
                                     parentHeight: Long
                                   ): BigInt =
    (BigInt(stakeAmount.toByteArray) * BigInt(difficulty) * BigInt(timeDelta)) /
      (BigInt(totalStake.toByteArray) * BigInt(
        supportedProtocolVersions.targetBlockTime(parentHeight).toUnit(MILLISECONDS).toLong
      ))

  /**
   * Calculate the block difficulty according to
   * [[https://nxtdocs.jelurida.com/Nxt_Whitepaper#Block_Creation_.28Forging.29]]
   *
   * @param prevDifficulty the previous base difficulty
   * @param prevTimes      sequence of block times to calculate the average and compare to target
   * @return the modified difficulty
   */
  private[consensus] def calcNewBaseDifficulty(newHeight: Long,
                                               prevDifficulty: Long,
                                               prevTimes: Seq[TimeProvider.Time]): Long = {

    val averageDelay = prevTimes.drop(1).lazyZip(prevTimes).map(_ - _).sum / (prevTimes.length - 1)
    val targetTimeMilli = supportedProtocolVersions.targetBlockTime(newHeight).toUnit(MILLISECONDS)

    // magic numbers here (1.1, 0.9, and 0.64) are straight from NXT
    if (averageDelay > targetTimeMilli) {
      (prevDifficulty * min(averageDelay.toDouble, targetTimeMilli * 1.1) / targetTimeMilli).toLong
    } else {
      (prevDifficulty * (1 - 0.64 * (1 - (max(averageDelay.toDouble, targetTimeMilli * 0.9) / targetTimeMilli)))).toLong
    }
  }
}

object NxtLeaderElection {
  // number of blocks to use for determining the avg block delay
  def nxtBlockNum: Int = 3

  def apply(settings: AppSettings): NxtLeaderElection =
    new NxtLeaderElection(ProtocolVersioner(settings.application.version, settings.forging.protocolVersions))
}
