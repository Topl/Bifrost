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

// TODO: Jing - remove
//
//  val typeBytes: Array[Byte] = "TransferTransaction".getBytes
//
//  val prefixBytes: Array[Byte] = Ints.toByteArray(typeBytes.length) ++ typeBytes
//
//  override def toBytes(m: TransferTransaction): Array[Byte] = {
//    prefixBytes ++
//      (m match {
//        case sc: PolyTransfer => PolyTransferSerializer.toChildBytes(sc)
//        case ac: ArbitTransfer => ArbitTransferSerializer.toChildBytes(ac)
//        case at: AssetTransfer => AssetTransferSerializer.toChildBytes(at)
//      })
//  }
//
//  override def parseBytes(bytes: Array[Byte]): Try[TransferTransaction] = Try {
//
//    val typeLength = Ints.fromByteArray(bytes.take(Ints.BYTES))
//    val typeStr = new String(bytes.slice(Ints.BYTES, Ints.BYTES + typeLength))
//
//    require(typeStr == "TransferTransaction")
//
//    val newBytes = bytes.slice(Ints.BYTES + typeLength, bytes.length)
//
//    val newTypeLength = Ints.fromByteArray(newBytes.slice(0, Ints.BYTES))
//    val newTypeStr = new String(newBytes.slice(Ints.BYTES, Ints.BYTES + newTypeLength))
//
//    newTypeStr match {
//      case "PolyTransfer" => PolyTransferSerializer.parseBytes(newBytes).get
//      case "ArbitTransfer" => ArbitTransferSerializer.parseBytes(newBytes).get
//      case "AssetTransfer" => AssetTransferSerializer.parseBytes(newBytes).get
//    }
//  }
}
