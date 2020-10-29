package co.topl.modifier.transaction.serialization

import co.topl.attestation.proposition.{PublicKeyCurve25519Proposition, PublicKeyCurve25519PropositionSerializer}
import co.topl.attestation.proof.{SignatureCurve25519, SignatureCurve25519Serializer}
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.modifier.transaction.Coinbase
import co.topl.utils.Extensions._
import co.topl.utils.serialization.{BifrostSerializer, Reader, Writer}

object CoinbaseSerializer extends BifrostSerializer[Coinbase] {

  override def serialize( obj: Coinbase, w: Writer): Unit = {
    /* to: IndexedSeq[(PublicKey25519Proposition, Long)] */
    w.putUInt(obj.to.length)
    obj.to.foreach { case (prop, value) =>
      PublicKeyCurve25519PropositionSerializer.serialize(prop, w)
      w.putULong(value)
    }

    /* signatures: IndexedSeq[Signature25519] */
    w.putUInt(obj.signatures.size)
    obj.signatures.foreach { case (prop, sig) =>
      PublicKeyCurve25519PropositionSerializer.serialize(prop, w)
      SignatureCurve25519Serializer.serialize(sig, w)
    }

    /* timestamp: Long */
    w.putULong(obj.timestamp)

    /* blockID: ModifierId */
    w.putBytes(obj.parentId.hashBytes)
  }

  override def parse(r: Reader): Coinbase = {
    val toLength: Int = r.getUInt().toIntExact
    val to: IndexedSeq[(PublicKeyCurve25519Proposition, Long)] = (0 until toLength).map { _ =>
      val prop = PublicKeyCurve25519PropositionSerializer.parse(r)
      val value = r.getULong()
      prop -> value
    }

    val signaturesLength: Int = r.getUInt().toIntExact
    val signatures: Map[PublicKeyCurve25519Proposition, SignatureCurve25519] = (0 until signaturesLength).map { _ =>
      val prop = PublicKeyCurve25519PropositionSerializer.parse(r)
      val sig = SignatureCurve25519Serializer.parse(r)
      prop -> sig
    }.toMap

    val timestamp: Long = r.getULong()

    val blockId: ModifierId = ModifierId(r.getBytes(Block.blockIdLength))

    Coinbase(to, signatures, timestamp, blockId)
  }
}
