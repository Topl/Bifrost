package bifrost.wallet

import bifrost.modifier.ModifierId
import bifrost.nodeView.NodeViewModifier
import bifrost.nodeView.box.GenericBox
import bifrost.nodeView.box.proposition.Proposition
import bifrost.utils.serialization.{ BifrostSerializer, Reader, Writer }

class WalletBoxSerializer[T, P <: Proposition, B <: GenericBox[P, T]](subclassDeser: BifrostSerializer[B])
  extends BifrostSerializer[WalletBox[T, P, B]] {

  override def serialize(obj: WalletBox[T, P, B], w: Writer): Unit = {
    /* box: B */
    subclassDeser.serialize(obj.box, w)

    /* transactionId: ModifierId */
    w.putBytes(obj.transactionId.hashBytes)

    /* createdAt: Long */
    w.putULong(obj.createdAt)
  }

  override def parse(r: Reader): WalletBox[T, P, B] = {
    val box: B = subclassDeser.parse(r)
    val transactionId: ModifierId = ModifierId(r.getBytes(NodeViewModifier.ModifierIdSize))
    val createdAt: Long = r.getULong()

    WalletBox[T, P, B](box, transactionId, createdAt)(subclassDeser)
  }
}
