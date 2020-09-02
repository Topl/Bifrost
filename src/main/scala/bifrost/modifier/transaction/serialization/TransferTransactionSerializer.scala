package bifrost.modifier.transaction.serialization

import bifrost.modifier.transaction.bifrostTransaction.{ArbitTransfer, AssetTransfer, PolyTransfer, TransferTransaction}
import bifrost.utils.serialization.{BifrostSerializer, Reader, Writer}
import com.google.common.primitives.Ints

import scala.util.Try

object TransferTransactionSerializer extends BifrostSerializer[TransferTransaction] {

  override def serialize(obj: TransferTransaction, w: Writer): Unit = {
    obj match {
      case sc: PolyTransfer =>
        w.putByteString("PolyTransfer")
        PolyTransferSerializer.serialize(sc, w)
      case ac: ArbitTransfer =>
        w.putByteString("ArbitTransfer")
        ArbitTransferSerializer.serialize(ac, w)
      case at: AssetTransfer =>
        w.putByteString("AssetTransfer")
        AssetTransferSerializer.serialize(at, w)
    }
  }

  override def parse(r: Reader): TransferTransaction = {
    r.getByteString() match {
      case "PolyTransfer" => PolyTransferSerializer.parse(r)
      case "ArbitTransfer" => ArbitTransferSerializer.parse(r)
      case "AssetTransfer" => AssetTransferSerializer.parse(r)
    }
  }
}
