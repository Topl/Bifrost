package bifrost

import bifrost.crypto.FastCryptographicHash
import bifrost.modifier.block.Block
import bifrost.modifier.box.ArbitBox
import com.google.common.primitives.Longs

import scala.concurrent.duration._
import scala.math.{max, min}

package object consensus {
  // TODO: JAA - 2020.07.21 - This is the maximum number of Arbits that are issued. It probably shouldn't be
  // TODO: hard-coded
  private val MaxTarget: Long = 5000000000L
  private var targetBlockTime: FiniteDuration = FiniteDuration(5, "seconds")

  def setBlockTime(blockTime: FiniteDuration): Unit = {
    this.targetBlockTime = blockTime
  }

  def calcHit(lastBlock: Block)(box: ArbitBox): Long = {
    val h = FastCryptographicHash(lastBlock.bytes ++ box.bytes)

    Longs.fromByteArray((0: Byte) +: h.take(7))
  }

  def calcAdjustedTarget(parent: Block,
                         difficulty: Long,
                         targetBlockDelay: FiniteDuration,
                         timestamp: Long): BigDecimal = {

    val target: Double = difficulty.toDouble / MaxTarget.toDouble
    val timeDelta = timestamp - parent.timestamp

    BigDecimal(target * timeDelta.toDouble / targetBlockDelay.toUnit(MILLISECONDS))
  }

  /**
    * Calculate the block difficulty according to
    * https://nxtdocs.jelurida.com/Nxt_Whitepaper#Block_Creation_.28Forging.29
    *
    * @param prevDifficulty the previous base difficulty
    * @param prevTimes      sequence of block times to calculate the average and compare to target
    * @return the modified difficulty
    */
  def calcNewBaseDifficulty(prevDifficulty: Long, prevTimes: Seq[Block.Timestamp]): Long = {
    val averageDelay = (prevTimes drop 1, prevTimes).zipped.map(_-_).sum / (prevTimes.length - 1)
    val targetTime = targetBlockTime.toUnit(MILLISECONDS)

    // magic numbers here (1.1, 0.9, and 0.64) are straight from NXT
    if (averageDelay > targetTime) {
      (prevDifficulty * min(averageDelay, targetTime * 1.1) / targetTime).toLong
    } else {
      (prevDifficulty * (1 - 0.64 * (1 - (max(averageDelay, targetTime * 0.9) / targetTime) ))).toLong
    }
  }
}
