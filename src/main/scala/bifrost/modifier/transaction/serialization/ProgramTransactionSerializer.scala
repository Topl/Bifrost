package bifrost.modifier.transaction.serialization

import bifrost.crypto.Signature25519
import bifrost.modifier.box.proposition.{Constants25519, PublicKey25519Proposition}
import bifrost.modifier.transaction.bifrostTransaction.Transaction.Nonce
import bifrost.modifier.transaction.bifrostTransaction._
import bifrost.utils.serialization.{BifrostSerializer, Reader, Writer}
import com.google.common.primitives.{Bytes, Ints, Longs}
import scorex.crypto.signatures.Curve25519

import scala.util.Try

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
