package bifrost.modifier.transaction.serialization

import bifrost.modifier.transaction.bifrostTransaction._
import bifrost.utils.serialization.{BifrostSerializer, Reader, Writer}
import com.google.common.primitives.Ints

import scala.util.Try

object TransactionSerializer extends BifrostSerializer[Transaction] {

  override def serialize(obj: Transaction, w: Writer): Unit = {
    obj match {
      case c: ProgramTransaction =>
        w.putByteString("ProgramTransaction")
        ProgramTransactionSerializer.serialize(c, w)
      case prT: ProgramTransfer =>
        w.putByteString("ProgramTransfer")
        ProgramTransferSerializer.serialize(prT, w)
      case p: TransferTransaction =>
        w.putByteString("TransferTransaction")
        TransferTransactionSerializer.serialize(p, w)
      case ac: AssetCreation =>
        w.putByteString("AssetCreation")
        AssetCreationSerializer.serialize(ac, w)
      case cb: CoinbaseTransaction =>
        w.putByteString("CoinbaseTransaction")
        CoinbaseTransactionSerializer.serialize(cb, w)
    }
  }

  override def parse(r: Reader): Transaction = {
    r.getByteString() match {
      case "ProgramTransaction" => ProgramTransactionSerializer.parse(r)
      case "ProgramTransfer" => ProgramTransferSerializer.parse(r)
      case "TransferTransaction" => TransferTransactionSerializer.parse(r)
      case "AssetCreation" => AssetCreationSerializer.parse(r)
      case "CoinbaseTransaction" => CoinbaseTransactionSerializer.parse(r)
    }
  }

// TODO: Jing - remove
//
//  override def toBytes(m: Transaction): Array[Byte] = m match {
//    case c: ProgramTransaction => ProgramTransactionSerializer.toBytes(c)
//    case prT: ProgramTransfer => ProgramTransferSerializer.toBytes(prT)
//    case p: TransferTransaction => TransferTransactionSerializer.toBytes(p)
//    case ac: AssetCreation => AssetCreationSerializer.toBytes(ac)
//    case cb: CoinbaseTransaction => CoinbaseTransactionSerializer.toBytes(cb)
//  }
//
//  override def parseBytes(bytes: Array[Byte]): Try[Transaction] = Try {
//    val typeLength = Ints.fromByteArray(bytes.slice(0, Ints.BYTES))
//    val typeStr = new String(bytes.slice(Ints.BYTES, Ints.BYTES + typeLength))
//
//    typeStr match {
//      case "ProgramTransaction" => ProgramTransactionSerializer.parseBytes(bytes).get
//      case "ProgramTransfer" => ProgramTransferSerializer.parseBytes(bytes).get
//      case "TransferTransaction" => TransferTransactionSerializer.parseBytes(bytes).get
//      case "AssetCreation" => AssetCreationSerializer.parseBytes(bytes).get
//      case "CoinbaseTransaction" => CoinbaseTransactionSerializer.parseBytes(bytes).get
//    }
//  }
}
