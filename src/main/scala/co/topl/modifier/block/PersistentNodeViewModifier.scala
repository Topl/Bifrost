package co.topl.modifier.block

import co.topl.modifier.transaction.Transaction
import co.topl.modifier.{ModifierId, NodeViewModifier}
import co.topl.utils.serialization.{BifrostSerializer, Writer}
import io.circe.Encoder

trait PersistentNodeViewModifier extends NodeViewModifier {

  type M = PersistentNodeViewModifier
  lazy val serializer: BifrostSerializer[PersistentNodeViewModifier] = PersistentNodeViewModifier

  def parentId: ModifierId
}

object PersistentNodeViewModifier extends BifrostSerializer[PersistentNodeViewModifier] {

  override def serialize(obj: PersistentNodeViewModifier, w: Writer): Unit = {
    obj match {
      case obj: Block =>
        w.put(Block.modifierTypeId)
        BlockSerializer.serialize(obj, w)

      case obj: PolyTransfer[_] =>
        w.put(PolyTransfer.txTypePrefix)
        PolyTransferSerializer.serialize(obj, w)

      case obj: AssetTransfer[_] =>
        w.put(AssetTransfer.txTypePrefix)
        AssetTransferSerializer.serialize(obj, w)

    }
  }

  override def parse(r: Reader): Transaction.TX = {
    (r.getByte() match {
      case ArbitTransfer.txTypePrefix => ArbitTransferSerializer.parseTry(r)
      case PolyTransfer.txTypePrefix  => PolyTransferSerializer.parseTry(r)
      case AssetTransfer.txTypePrefix  => AssetTransferSerializer.parseTry(r)
    }) match {
      case Success(tx) => tx
      case Failure(ex) => throw ex
    }
  }

  implicit val jsonEncoder: Encoder[PersistentNodeViewModifier] = {
    case b: Block ⇒ Block.jsonEncoder(b)
    case other => throw new Exception(s"Unknown persistent modifier type: $other")
  }
}
