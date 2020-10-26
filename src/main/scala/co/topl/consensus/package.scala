package co.topl

import co.topl.crypto.FastCryptographicHash
import co.topl.modifier.block.Block
import co.topl.nodeView.state.box.ArbitBox
import co.topl.settings.ProtocolSettings
import com.google.common.primitives.Longs

import scala.concurrent.duration._
import scala.math.{ max, min }

package object consensus {
  private var _protocolMngr: ProtocolVersioner = ProtocolVersioner.empty

  // these variables are left as vars since they are local state of the consensus protocol determined from the chain
  private var _maxStake: Long = _
  private var _inflation: Long = _  // not currently used
  private var _difficulty: Long = _ // not currently used
  private var _height: Long = _     // not currently used

  // setters
  private[consensus] def protocolMngr_= (value: ProtocolVersioner): Unit = _protocolMngr = value
  private[consensus] def maxStake_= (value: Long): Unit = _maxStake = value
  private[consensus] def inflation_= (value: Long): Unit = _inflation = value
  private[consensus] def height_= (value: Long): Unit = _height = value
  private[consensus] def difficulty_= (value: Long): Unit = _difficulty = value

  // getters
  def protocolMngr: ProtocolVersioner = _protocolMngr
  def maxStake: Long = _maxStake
  def inflation: Long = _inflation
  def difficulty: Long = _difficulty
  def height: Long = _height

  // number of blocks to use for determining the avg block delay
  def nxtBlockNum: Int = 3

  /** Find the rule set for the given app version and block height */
  def getProtocolRules(blockHeight: Long): ProtocolSettings =
    protocolMngr.current(blockHeight)
    .getOrElse(throw new Error("Unable to find applicable protocol rules"))

  def targetBlockTime(blockHeight: Long): FiniteDuration =
    getProtocolRules(blockHeight).targetBlockTime.get

  def numTxInBlock(blockHeight: Long): Int =
    getProtocolRules(blockHeight).numTxPerBlock.get

  def blockVersion(blockHeight: Long): Byte =
    getProtocolRules(blockHeight).blockVersion.get

  /**
   * Defines how we calculate the test value for determining eligibility to forge
   *
   * @param lastBlock previous block
   * @param box       box to be used for the test value
   * @return the test value to be compared to the adjusted difficulty
   */
  def calcHit(lastBlock: Block)(box: ArbitBox): Long = {
    val h = FastCryptographicHash(lastBlock.bytes ++ box.bytes)

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
  def calcAdjustedTarget(parent: Block,
                         parentHeight: Long,
                         baseDifficulty: Long,
                         timestamp: Long): BigDecimal = {

    val target: Double = baseDifficulty.toDouble / maxStake.toDouble
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
  def calcNewBaseDifficulty(newHeight: Long, prevDifficulty: Long, prevTimes: Seq[Block.Timestamp]): Long = {
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
