package bifrost

import java.time.Instant

import bifrost.crypto.{FastCryptographicHash, PrivateKey25519}
import bifrost.modifier.block.Block
import bifrost.modifier.box.ArbitBox
import bifrost.modifier.transaction.bifrostTransaction.{CoinbaseTransaction, Transaction}
import bifrost.utils.Logging
import com.google.common.primitives.Longs

import scala.concurrent.duration._

package object consensus extends Logging {
  private val MaxTarget: Long = Long.MaxValue

  def calcAdjustedTarget(difficulty: Long,
                         parent: Block,
                         targetBlockDelay: FiniteDuration): BigInt = {

    val target: Double = MaxTarget.toDouble / difficulty.toDouble
    val timeDelta = Instant.now().toEpochMilli - parent.timestamp

    BigDecimal(target * timeDelta.toDouble / targetBlockDelay.toUnit(MILLISECONDS)).toBigInt()
  }

  def calcHit(lastBlock: Block)(box: ArbitBox): Long = {
    val h = FastCryptographicHash(lastBlock.bytes ++ box.bytes)
    Longs.fromByteArray((0: Byte) +: h.take(7))
  }

  def iteration(parent: Block,
                boxKeys: Seq[(ArbitBox, PrivateKey25519)],
                txsToInclude: Seq[Transaction],
                target: BigInt,
                version: Block.Version): Option[Block] = {

    // fixme: JAA - would prefer not to have logging here but leaving for now (2020.07.17)
    log.debug("in the iteration function")

    val successfulHits = boxKeys.map { boxKey =>
      val h = calcHit(parent)(boxKey._1)
      //log.debug(s"Hit value: $h")
      (boxKey, h)
    }.filter(t => BigInt(t._2) < BigInt(t._1._1.value) * target)

    log.debug(s"Successful hits: ${successfulHits.size}")
    successfulHits.headOption.map { case (boxKey, _) =>
      if (txsToInclude.head.asInstanceOf[CoinbaseTransaction].newBoxes.nonEmpty) {
        Block.create(parent.id, Instant.now().toEpochMilli, txsToInclude, boxKey._1, boxKey._2,
          txsToInclude.head.asInstanceOf[CoinbaseTransaction].newBoxes.head.asInstanceOf[ArbitBox].value, version) // inflation val
      }
      else {
        Block.create(parent.id, Instant.now().toEpochMilli, txsToInclude, boxKey._1, boxKey._2, 0, version)
      }
    }
  }
}
