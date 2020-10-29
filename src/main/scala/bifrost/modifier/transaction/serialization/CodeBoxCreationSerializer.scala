package bifrost.modifier.transaction.serialization

import bifrost.crypto.Signature25519
import bifrost.crypto.serialization.Signature25519Serializer
import bifrost.modifier.box.proposition.{PublicKey25519Proposition, PublicKey25519PropositionSerializer}
import bifrost.modifier.transaction.bifrostTransaction.CodeCreation
import bifrost.utils.serialization.{BifrostSerializer, Reader, Writer}

object CodeBoxCreationSerializer extends BifrostSerializer[CodeCreation] {

  override def serialize(obj: CodeCreation, w: Writer): Unit = {
    w.putByteString("CodeCreation")

    /* to: PublicKey25519Proposition */
    PublicKey25519PropositionSerializer.serialize(obj.to, w)

    /* signature: Signature25519 */
    Signature25519Serializer.serialize(obj.signature, w)

    /* code: String */
    w.putIntString(obj.code)

    /* override val fee: Long */
    w.putULong(obj.fee)

    /* override val timestamp: Long */
    w.putULong(obj.timestamp)

    /* data: String */
    w.putIntString(obj.data)
  }

  override def parse(r: Reader): CodeCreation = {
    require(r.getByteString() == "CodeCreation")

    val to: PublicKey25519Proposition = PublicKey25519PropositionSerializer.parse(r)
    val signature: Signature25519 = Signature25519Serializer.parse(r)
    val code: String = r.getIntString()
    val fee: Long = r.getULong()
    val timestamp: Long = r.getULong()
    val data: String = r.getIntString()

    CodeCreation(to, signature, code, fee, timestamp, data)
  }
}
