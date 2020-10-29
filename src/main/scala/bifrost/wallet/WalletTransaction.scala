package bifrost.wallet

import bifrost.modifier.ModifierId
import bifrost.modifier.box.proposition.Proposition
import bifrost.modifier.transaction.bifrostTransaction.GenericTransaction
import bifrost.nodeView.NodeViewModifier
import com.google.common.primitives.{Bytes, Ints, Longs}

import scala.util.Try

case class WalletTransaction[P <: Proposition, TX <: GenericTransaction[P]](
  proposition: P,
  tx: TX,
  blockId: Option[ModifierId],
  createdAt: Long
)

object WalletTransaction {

  def parse[P <: Proposition, TX <: GenericTransaction[P]](
    bytes: Array[Byte]
  )(propDeserializer: Array[Byte] => Try[P], txDeserializer: Array[Byte] => Try[TX]): Try[WalletTransaction[P, TX]] = Try {
    val propLength = Ints.fromByteArray(bytes.slice(0, 4))
    var pos = 4
    val propTry = propDeserializer(bytes.slice(pos, pos + propLength))
    pos = pos + propLength

    val txLength = Ints.fromByteArray(bytes.slice(pos, pos + 4))
    val txTry = txDeserializer(bytes.slice(pos, pos + txLength))
    pos = pos + txLength

    val blockIdOpt: Option[ModifierId] =
      if (bytes.slice(pos, pos + 1).head == 0) {
        pos = pos + 1
        None
      } else {
        val o = bytes.slice(pos + 1, pos + 1 + NodeViewModifier.ModifierIdSize)
        pos = pos + 1 + NodeViewModifier.ModifierIdSize
        Some(ModifierId(o))
      }

    val createdAt = Longs.fromByteArray(bytes.slice(pos, pos + 8))

    WalletTransaction[P, TX](propTry.get, txTry.get, blockIdOpt, createdAt)
  }

  def bytes[P <: Proposition, TX <: GenericTransaction[P]](wt: WalletTransaction[P, TX]): Array[Byte] = {
    val propBytes = wt.proposition.bytes
    val txBytes = wt.tx.bytes
    val bIdBytes = wt.blockId.map(id => Array(1: Byte) ++ id.hashBytes).getOrElse(Array(0: Byte))

    Bytes.concat(
      Ints.toByteArray(propBytes.length),
      propBytes,
      Ints.toByteArray(txBytes.length),
      txBytes,
      bIdBytes,
      Longs.toByteArray(wt.createdAt)
    )
  }
}
