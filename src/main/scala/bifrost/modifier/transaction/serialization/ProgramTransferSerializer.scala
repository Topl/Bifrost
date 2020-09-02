package bifrost.modifier.transaction.serialization

import bifrost.crypto.Signature25519
import bifrost.crypto.serialization.Signature25519Serializer
import bifrost.modifier.box.ExecutionBox
import bifrost.modifier.box.proposition.{Constants25519, PublicKey25519Proposition, PublicKey25519PropositionSerializer}
import bifrost.modifier.box.serialization.ExecutionBoxSerializer
import bifrost.modifier.transaction.bifrostTransaction.ProgramTransfer
import bifrost.utils.serialization.{BifrostSerializer, Reader, Writer}
import com.google.common.primitives.{Bytes, Ints, Longs}
import scorex.crypto.signatures.Curve25519

import scala.util.Try

object ProgramTransferSerializer extends BifrostSerializer[ProgramTransfer]{

  override def serialize(obj: ProgramTransfer, w: Writer): Unit = {
    /* from: PublicKey25519Proposition */
    PublicKey25519PropositionSerializer.serialize(obj.from, w)

    /* to: PublicKey25519Proposition */
    PublicKey25519PropositionSerializer.serialize(obj.to, w)

    /* signature: Signature25519 */
    Signature25519Serializer.serialize(obj.signature, w)

    /* executionBox: ExecutionBox */
    ExecutionBoxSerializer.serialize(obj.executionBox, w)

    /* fee: Long */
    w.putULong(obj.fee)

    /* timestamp: Long */
    w.putULong(obj.timestamp)

    /* data: String */
    w.putIntString(obj.data)
  }

  override def parse(r: Reader): ProgramTransfer = {
    val from: PublicKey25519Proposition = PublicKey25519PropositionSerializer.parse(r)
    val to: PublicKey25519Proposition = PublicKey25519PropositionSerializer.parse(r)
    val signature: Signature25519 = Signature25519Serializer.parse(r)
    val executionBox: ExecutionBox = ExecutionBoxSerializer.parse(r)
    val fee: Long = r.getULong()
    val timestamp: Long = r.getULong()
    val data: String = r.getIntString()

    ProgramTransfer(from, to, signature, executionBox, fee, timestamp, data)
  }
}
