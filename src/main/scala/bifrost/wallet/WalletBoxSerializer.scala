package bifrost.wallet

import bifrost.modifier.box.GenericBox
import bifrost.modifier.box.proposition.Proposition
import bifrost.nodeView.NodeViewModifier
import bifrost.serialization.Serializer
import com.google.common.primitives.{Bytes, Longs}

import scala.util.Try

class WalletBoxSerializer[T, P <: Proposition, B <: GenericBox[P, T]](subclassDeser: Serializer[B])
  extends Serializer[WalletBox[T, P, B]] {

  override def toBytes(box: WalletBox[T, P, B]): Array[Byte] = {
    Bytes.concat(box.transactionId, Longs.toByteArray(box.createdAt), box.box.bytes)
  }

  override def parseBytes(bytes: Array[Byte]): Try[WalletBox[T, P, B]] = Try {
    val txId = bytes.slice(0, NodeViewModifier.ModifierIdSize)
    val createdAt = Longs.fromByteArray(
      bytes.slice(NodeViewModifier.ModifierIdSize, NodeViewModifier.ModifierIdSize + 8))
    val boxB = bytes.slice(NodeViewModifier.ModifierIdSize + 8, bytes.length)
    val box: B = subclassDeser.parseBytes(boxB).get
    WalletBox[T, P, B](box, txId, createdAt)(subclassDeser)
  }
}
