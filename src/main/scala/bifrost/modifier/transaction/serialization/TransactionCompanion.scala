package bifrost.modifier.transaction.serialization

import bifrost.modifier.transaction.bifrostTransaction._
import bifrost.utils.serialization.{BifrostSerializer, Reader, Writer}
import com.google.common.primitives.Ints

import scala.util.Try

object TransactionCompanion extends BifrostSerializer[Transaction] {

  override def toBytes(m: Transaction): Array[Byte] = m match {
    case c: ProgramTransaction => ProgramTransactionCompanion.toBytes(c)
    case prT: ProgramTransfer => ProgramTransferCompanion.toBytes(prT)
    case p: TransferTransaction => TransferTransactionCompanion.toBytes(p)
    case ac: AssetCreation => AssetCreationCompanion.toBytes(ac)
    case cb: CoinbaseTransaction => CoinbaseTransactionCompanion.toBytes(cb)
  }

  override def parseBytes(bytes: Array[Byte]): Try[Transaction] = Try {
    val typeLength = Ints.fromByteArray(bytes.slice(0, Ints.BYTES))
    val typeStr = new String(bytes.slice(Ints.BYTES, Ints.BYTES + typeLength))

    typeStr match {
      case "ProgramTransaction" => ProgramTransactionCompanion.parseBytes(bytes).get
      case "ProgramTransfer" => ProgramTransferCompanion.parseBytes(bytes).get
      case "TransferTransaction" => TransferTransactionCompanion.parseBytes(bytes).get
      case "AssetCreation" => AssetCreationCompanion.parseBytes(bytes).get
      case "CoinbaseTransaction" => CoinbaseTransactionCompanion.parseBytes(bytes).get
    }
  }

  override def serialize(obj: Transaction, w: Writer): Unit = ???

  override def parse(r: Reader): Transaction = {
    ???
  }
}
