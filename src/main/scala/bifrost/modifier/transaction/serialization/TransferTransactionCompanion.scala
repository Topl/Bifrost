package bifrost.modifier.transaction.serialization

import bifrost.modifier.transaction.bifrostTransaction.{ArbitTransfer, AssetTransfer, PolyTransfer, TransferTransaction}
import bifrost.utils.serialization.{BifrostSerializer, Reader, Writer}
import com.google.common.primitives.Ints

import scala.util.Try

object TransferTransactionCompanion extends BifrostSerializer[TransferTransaction] {

  override def serialize(obj: TransferTransaction, w: Writer): Unit = {
    obj match {
      case sc: PolyTransfer =>
        w.putByteString("PolyTransfer")
        PolyTransferCompanion.serialize(sc, w)
      case ac: ArbitTransfer =>
        w.putByteString("ArbitTransfer")
        ArbitTransferCompanion.serialize(ac, w)
      case at: AssetTransfer =>
        w.putByteString("AssetTransfer")
        AssetTransferCompanion.serialize(at, w)
    }
  }

  override def parse(r: Reader): TransferTransaction = {
    r.getByteString() match {
      case "PolyTransfer" => PolyTransferCompanion.parse(r)
      case "ArbitTransfer" => ArbitTransferCompanion.parse(r)
      case "AssetTransfer" => AssetTransferCompanion.parse(r)
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
//        case sc: PolyTransfer => PolyTransferCompanion.toChildBytes(sc)
//        case ac: ArbitTransfer => ArbitTransferCompanion.toChildBytes(ac)
//        case at: AssetTransfer => AssetTransferCompanion.toChildBytes(at)
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
//      case "PolyTransfer" => PolyTransferCompanion.parseBytes(newBytes).get
//      case "ArbitTransfer" => ArbitTransferCompanion.parseBytes(newBytes).get
//      case "AssetTransfer" => AssetTransferCompanion.parseBytes(newBytes).get
//    }
//  }
}
