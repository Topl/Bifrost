package co.topl.modifier.transaction.serialization

import co.topl.crypto.signature.serialization.Signature25519Serializer
import co.topl.modifier.transaction.ProgramTransfer
import co.topl.nodeView.state.box.ExecutionBox
import co.topl.crypto.proposition.PublicKey25519Proposition
import co.topl.crypto.proposition.serialization.PublicKey25519PropositionSerializer
import co.topl.crypto.signature.Signature25519
import co.topl.nodeView.state.box.serialization.ExecutionBoxSerializer
import co.topl.utils.serialization.{ BifrostSerializer, Reader, Writer }

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
