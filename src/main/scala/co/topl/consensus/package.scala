package co.topl

import co.topl.crypto.FastCryptographicHash
import co.topl.modifier.block.Block
import co.topl.nodeView.state.box.ArbitBox
import com.google.common.primitives.Longs

import scala.concurrent.duration._
import scala.math.{max, min}

package object consensus {
  // TODO: JAA - 2020.07.21 - This is the maximum number of Arbits that are issued. It probably shouldn't be
  // TODO: hard-coded
  // these variables are left as vars since they need to be determined at runtime from the network config
  // todo: JAA - figure out a better way to do this
  private var _maxStake: Long = 5000000000L
  private var _inflation: Long = 0L
  private var _targetBlockTime: FiniteDuration = FiniteDuration(5, "seconds")

  // setters
  private[consensus] def maxStake_= (value: Long): Unit = _maxStake = value
  private[consensus] def inflation_= (value: Long): Unit = _inflation = value
  private[consensus] def targetBlockTime_= (value: FiniteDuration): Unit = _targetBlockTime = value

  // getters
  def maxStake: Long = _maxStake
  def inflation: Long = _inflation
  def targetBlockTime: FiniteDuration = _targetBlockTime

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
                         baseDifficulty: Long,
                         timestamp: Long): BigDecimal = {

    val target: Double = baseDifficulty.toDouble / maxStake.toDouble
    val timeDelta = timestamp - parent.timestamp

    BigDecimal(target * timeDelta.toDouble / targetBlockTime.toUnit(MILLISECONDS))
  }

  /**
    * Calculate the block difficulty according to
    * [[https://nxtdocs.jelurida.com/Nxt_Whitepaper#Block_Creation_.28Forging.29]]
    *
    * @param prevDifficulty the previous base difficulty
    * @param prevTimes      sequence of block times to calculate the average and compare to target
    * @return the modified difficulty
    */
  def calcNewBaseDifficulty(prevDifficulty: Long, prevTimes: Seq[Block.Timestamp]): Long = {
    val averageDelay = (prevTimes drop 1, prevTimes).zipped.map(_-_).sum / (prevTimes.length - 1)
    val targetTimeMilli = targetBlockTime.toUnit(MILLISECONDS)

    // magic numbers here (1.1, 0.9, and 0.64) are straight from NXT
    if (averageDelay > targetTimeMilli) {
      (prevDifficulty * min(averageDelay, targetTimeMilli * 1.1) / targetTimeMilli).toLong
    } else {
      (prevDifficulty * (1 - 0.64 * (1 - (max(averageDelay, targetTimeMilli * 0.9) / targetTimeMilli) ))).toLong
    }
  }
}
