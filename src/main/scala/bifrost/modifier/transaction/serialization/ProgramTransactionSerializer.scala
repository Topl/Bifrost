package bifrost.modifier.transaction.serialization

import bifrost.modifier.transaction.bifrostTransaction._
import bifrost.utils.serialization.{BifrostSerializer, Reader, Writer}

object ProgramTransactionSerializer extends BifrostSerializer[ProgramTransaction] {

  override def serialize(obj: ProgramTransaction, w: Writer): Unit = {
    obj match {
      case cc: ProgramCreation =>
        w.putByteString("ProgramCreation")
        ProgramCreationSerializer.serialize(cc, w)
      case cme: ProgramMethodExecution =>
        w.putByteString("ProgramMethodExecution")
        ProgramMethodExecutionSerializer.serialize(cme, w)
    }
  }

  override def parse(r: Reader): ProgramTransaction = {
    r.getByteString() match {
      case "ProgramCreation" => ProgramCreationSerializer.parse(r)
      case "ProgramMethodExecution" => ProgramMethodExecutionSerializer.parse(r)
    }
  }
}
