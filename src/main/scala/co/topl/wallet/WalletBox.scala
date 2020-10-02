package co.topl.wallet

import co.topl.modifier.ModifierId
import co.topl.nodeView.box.GenericBox
import co.topl.nodeView.box.proposition.Proposition
import co.topl.utils.serialization.{ BifrostSerializer, BytesSerializable }

case class WalletBox[T, P <: Proposition, B <: GenericBox[P, T]]
  (box: B, transactionId: ModifierId, createdAt: Long)(subclassDeser: BifrostSerializer[B])
  extends BytesSerializable {

  override type M = WalletBox[T, P, B]

  override def serializer: BifrostSerializer[WalletBox[T, P, B]] =
    new WalletBoxSerializer[T, P, B](subclassDeser)

  override def toString: String = s"WalletBox($box, ${transactionId.toString}, $createdAt)"
}
