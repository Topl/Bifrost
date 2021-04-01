package co.topl

import co.topl.modifier.block.Block
import co.topl.modifier.box.ArbitBox
import co.topl.settings.ProtocolSettings
import co.topl.utils.{Int128, TimeProvider}
import com.google.common.primitives.Longs
import scorex.crypto.hash.Blake2b256

import scala.concurrent.duration._
import scala.math.{max, min}

package object consensus {
  private var _protocolMngr: ProtocolVersioner = ProtocolVersioner.empty

  // Initialize or restore a consensus storage that keeps track of the maxStake, difficulty, height, and inflation
  private[consensus] var consensusStorage: ConsensusStorage = ConsensusStorage.emptyStorage()

  // setter
  private[consensus] def protocolMngr_= (value: ProtocolVersioner): Unit = _protocolMngr = value

  // getters
  def protocolMngr: ProtocolVersioner = _protocolMngr

  // number of blocks to use for determining the avg block delay
  def nxtBlockNum: Int = 3

  /** Find the rule set for the given app version and block height */
  def getProtocolRules(blockHeight: Int128): ProtocolSettings =
    protocolMngr.current(blockHeight)
    .getOrElse(throw new Error("Unable to find applicable protocol rules"))

  def targetBlockTime(blockHeight: Int128): FiniteDuration =
    getProtocolRules(blockHeight).targetBlockTime.get

  def numTxInBlock(blockHeight: Int128): Int =
    getProtocolRules(blockHeight).numTxPerBlock.get

  def blockVersion(blockHeight: Int128): Byte =
    getProtocolRules(blockHeight).blockVersion.get

  /**
   * Defines how we calculate the test value for determining eligibility to forge
   *
   * @param lastBlock previous block
   * @param box       box to be used for the test value
   * @return the test value to be compared to the adjusted difficulty
   */
  def calcHit(lastBlock: Block)(box: ArbitBox): Long = {
    val h = Blake2b256(lastBlock.bytes ++ box.bytes)

    Longs.fromByteArray((0: Byte) +: h.take(7))
  }

  /**
   * Calculates the adjusted difficulty for forging based on the time passed since the previous block
   *
   * @param parent         previous block
   * @param baseDifficulty base difficulty of the parent block
   * @param timestamp      the current timestamp
   * @return the adjusted difficulty
   */
  def calcAdjustedTarget(parent: Block, parentHeight: Int128, baseDifficulty: Int128, timestamp: Long): BigDecimal = {

    val target: Double = baseDifficulty.toDouble / consensusStorage.totalStake.toDouble
    val timeDelta = timestamp - parent.timestamp

    BigDecimal(target * timeDelta.toDouble / targetBlockTime(parentHeight).toUnit(MILLISECONDS))
  }

  /**
    * Calculate the block difficulty according to
    * [[https://nxtdocs.jelurida.com/Nxt_Whitepaper#Block_Creation_.28Forging.29]]
    *
    * @param prevDifficulty the previous base difficulty
    * @param prevTimes      sequence of block times to calculate the average and compare to target
    * @return the modified difficulty
    */
  def calcNewBaseDifficulty(newHeight: Long, prevDifficulty: Long, prevTimes: Seq[TimeProvider.Time]): Long = {

    val averageDelay = (prevTimes drop 1, prevTimes).zipped.map(_-_).sum / (prevTimes.length - 1)
    val targetTimeMilli = targetBlockTime(newHeight).toUnit(MILLISECONDS)

    // magic numbers here (1.1, 0.9, and 0.64) are straight from NXT
    if (averageDelay > targetTimeMilli) {
      (prevDifficulty * min(averageDelay, targetTimeMilli * 1.1) / targetTimeMilli).toLong
    } else {
      (prevDifficulty * (1 - 0.64 * (1 - (max(averageDelay, targetTimeMilli * 0.9) / targetTimeMilli) ))).toLong
    }
  }
}
