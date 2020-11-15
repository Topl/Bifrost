package co.topl.modifier.transaction.serialization

import co.topl.modifier.transaction._
import co.topl.utils.serialization.{BifrostSerializer, Reader, Writer}

object TransactionSerializer extends BifrostSerializer[Transaction] {

  override def serialize(obj: Transaction, w: Writer): Unit = {
    obj match {
      case obj: CodeCreation =>
        w.putByteString("CodeCreation")
        CodeBoxCreationSerializer.serialize(obj, w)
      case obj: ProgramCreation =>
        w.putByteString("ProgramCreation")
        ProgramCreationSerializer.serialize(obj, w)
      case obj: ProgramMethodExecution =>
        w.putByteString("ProgramMethodExecution")
        ProgramMethodExecutionSerializer.serialize(obj, w)

      case obj: ProgramTransfer =>
        w.putByteString("ProgramTransfer")
        ProgramTransferSerializer.serialize(obj, w)

      case obj: PolyTransfer =>
        w.putByteString("PolyTransfer")
        PolyTransferSerializer.serialize(obj, w)
      case obj: ArbitTransfer =>
        w.putByteString("ArbitTransfer")
        ArbitTransferSerializer.serialize(obj, w)
      case obj: AssetTransfer =>
        w.putByteString("AssetTransfer")
        AssetTransferSerializer.serialize(obj, w)

      case obj: AssetCreation =>
        w.putByteString("AssetCreation")
        AssetCreationSerializer.serialize(obj, w)
      case obj: Coinbase      =>
        w.putByteString("Coinbase")
        CoinbaseSerializer.serialize(obj, w)
    }
  }

  override def parse(r: Reader): Transaction = {
    r.getByteString() match {
      case "CodeCreation" => CodeBoxCreationSerializer.parse(r)
      case "ProgramCreation" => ProgramCreationSerializer.parse(r)
      case "ProgramMethodExecution" => ProgramMethodExecutionSerializer.parse(r)

      case "ProgramTransfer" => ProgramTransferSerializer.parse(r)

      case "PolyTransfer" => PolyTransferSerializer.parse(r)
      case "ArbitTransfer" => ArbitTransferSerializer.parse(r)
      case "AssetTransfer" => AssetTransferSerializer.parse(r)

      case "AssetCreation" => AssetCreationSerializer.parse(r)
      case "Coinbase" => CoinbaseSerializer.parse(r)
    }
  }
}
