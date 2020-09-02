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
}
