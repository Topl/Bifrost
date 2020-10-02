package bifrost.wallet

import bifrost.modifier.ModifierId
import bifrost.nodeView.box.GenericBox
import bifrost.nodeView.box.proposition.Proposition
import bifrost.utils.serialization.{ BifrostSerializer, BytesSerializable }

case class WalletBox[T, P <: Proposition, B <: GenericBox[P, T]]
  (box: B, transactionId: ModifierId, createdAt: Long)(subclassDeser: BifrostSerializer[B])
  extends BytesSerializable {

  override type M = WalletBox[T, P, B]

  override def serializer: BifrostSerializer[WalletBox[T, P, B]] =
    new WalletBoxSerializer[T, P, B](subclassDeser)

  override def toString: String = s"WalletBox($box, ${transactionId.toString}, $createdAt)"
}
