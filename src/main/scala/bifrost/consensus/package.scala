package bifrost

import bifrost.crypto.FastCryptographicHash
import bifrost.modifier.block.Block
import bifrost.modifier.box.ArbitBox
import com.google.common.primitives.Longs

import scala.concurrent.duration._

package object consensus {
  private val MaxTarget: Long = Long.MaxValue

  def calcHit(lastBlock: Block)(box: ArbitBox): Long = {
    val h = FastCryptographicHash(lastBlock.bytes ++ box.bytes)
    Longs.fromByteArray((0: Byte) +: h.take(7))
  }

  def calcAdjustedTarget(parent: Block,
                         difficulty: Long,
                         targetBlockDelay: FiniteDuration,
                         timestamp: Long): BigInt = {

    val target: Double = MaxTarget.toDouble / difficulty.toDouble
    val timeDelta = timestamp - parent.timestamp

    BigDecimal(target * timeDelta.toDouble / targetBlockDelay.toUnit(MILLISECONDS)).toBigInt()
  }

}
