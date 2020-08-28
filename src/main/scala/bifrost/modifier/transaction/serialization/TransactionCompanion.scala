package bifrost.modifier.transaction.serialization

import bifrost.modifier.transaction.bifrostTransaction._
import bifrost.utils.serialization.{BifrostSerializer, Reader, Writer}
import com.google.common.primitives.Ints

import scala.util.Try

object TransactionCompanion extends BifrostSerializer[Transaction] {

  override def serialize(obj: Transaction, w: Writer): Unit = {
    obj match {
      case c: ProgramTransaction =>
        w.putByteString("ProgramTransaction")
        ProgramTransactionCompanion.serialize(c, w)
      case prT: ProgramTransfer =>
        w.putByteString("ProgramTransfer")
        ProgramTransferCompanion.serialize(prT, w)
      case p: TransferTransaction =>
        w.putByteString("TransferTransaction")
        TransferTransactionCompanion.serialize(p, w)
      case ac: AssetCreation =>
        w.putByteString("AssetCreation")
        AssetCreationCompanion.serialize(ac, w)
      case cb: CoinbaseTransaction =>
        w.putByteString("CoinbaseTransaction")
        CoinbaseTransactionCompanion.serialize(cb, w)
    }
  }

  override def parse(r: Reader): Transaction = {
    r.getByteString() match {
      case "ProgramTransaction" => ProgramTransactionCompanion.parse(r)
      case "ProgramTransfer" => ProgramTransferCompanion.parse(r)
      case "TransferTransaction" => TransferTransactionCompanion.parse(r)
      case "AssetCreation" => AssetCreationCompanion.parse(r)
      case "CoinbaseTransaction" => CoinbaseTransactionCompanion.parse(r)
    }
  }

// TODO: Jing - remove
//
//  override def toBytes(m: Transaction): Array[Byte] = m match {
//    case c: ProgramTransaction => ProgramTransactionCompanion.toBytes(c)
//    case prT: ProgramTransfer => ProgramTransferCompanion.toBytes(prT)
//    case p: TransferTransaction => TransferTransactionCompanion.toBytes(p)
//    case ac: AssetCreation => AssetCreationCompanion.toBytes(ac)
//    case cb: CoinbaseTransaction => CoinbaseTransactionCompanion.toBytes(cb)
//  }
//
//  override def parseBytes(bytes: Array[Byte]): Try[Transaction] = Try {
//    val typeLength = Ints.fromByteArray(bytes.slice(0, Ints.BYTES))
//    val typeStr = new String(bytes.slice(Ints.BYTES, Ints.BYTES + typeLength))
//
//    typeStr match {
//      case "ProgramTransaction" => ProgramTransactionCompanion.parseBytes(bytes).get
//      case "ProgramTransfer" => ProgramTransferCompanion.parseBytes(bytes).get
//      case "TransferTransaction" => TransferTransactionCompanion.parseBytes(bytes).get
//      case "AssetCreation" => AssetCreationCompanion.parseBytes(bytes).get
//      case "CoinbaseTransaction" => CoinbaseTransactionCompanion.parseBytes(bytes).get
//    }
//  }
}
