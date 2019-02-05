package bifrost.transaction.serialization

import bifrost.serialization.Serializer
import bifrost.transaction._
import bifrost.transaction.bifrostTransaction.{ArbitTransfer, AssetTransfer, PolyTransfer, TransferTransaction}
import com.google.common.primitives.Ints

import scala.util.Try

object TransferTransactionCompanion extends Serializer[TransferTransaction] {
  val typeBytes = "TransferTransaction".getBytes

  val prefixBytes = Ints.toByteArray(typeBytes.length) ++ typeBytes

  override def toBytes(m: TransferTransaction): Array[Byte] = {
    prefixBytes ++
      (m match {
        case sc: PolyTransfer => PolyTransferCompanion.toChildBytes(sc)
        case ac: ArbitTransfer => ArbitTransferCompanion.toChildBytes(ac)
        case at: AssetTransfer => AssetTransferCompanion.toChildBytes(at)
      })
  }

  override def parseBytes(bytes: Array[Byte]): Try[TransferTransaction] = Try {

    val typeLength = Ints.fromByteArray(bytes.take(Ints.BYTES))
    val typeStr = new String(bytes.slice(Ints.BYTES, Ints.BYTES + typeLength))

    val newBytes = bytes.slice(Ints.BYTES + typeLength, bytes.length)

    val newTypeLength = Ints.fromByteArray(newBytes.slice(0, Ints.BYTES))
    val newTypeStr = new String(newBytes.slice(Ints.BYTES, Ints.BYTES + newTypeLength))

    newTypeStr match {
      case "PolyTransfer" => PolyTransferCompanion.parseBytes(newBytes).get
      case "ArbitTransfer" => ArbitTransferCompanion.parseBytes(newBytes).get
      case "AssetTransfer" => AssetTransferCompanion.parseBytes(newBytes).get
    }
  }
}
