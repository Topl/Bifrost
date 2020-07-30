package bifrost

import bifrost.crypto.FastCryptographicHash
import bifrost.modifier.block.Block
import bifrost.modifier.box.ArbitBox
import com.google.common.primitives.Longs

import scala.concurrent.duration._

package object consensus {
  // TODO: JAA - 2020.07.21 - This is the maximum number of Arbits that are issued. It probably shouldn't be
  // TODO: hard-coded
  private val MaxTarget: Long = 5000000000L

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

}
