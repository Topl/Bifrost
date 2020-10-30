package co.topl.attestation.proposition

import co.topl.attestation.proof.{ SignatureCurve25519, SignatureCurve25519Serializer }
import co.topl.utils.Extensions._
import co.topl.utils.serialization.{ BifrostSerializer, Reader, Writer }
import scorex.crypto.signatures.{ Curve25519, PublicKey }

object ThresholdCurve25519PropositionSerializer extends BifrostSerializer[ThresholdCurve25519Proposition] {

  override def serialize(obj: ThresholdCurve25519Proposition, w: Writer): Unit = {
    /* m: Int */
    w.putUInt(obj.threshold)

    /* setOfPubKeyBytes: Set[Array[Byte]] */
    w.putUInt(obj.pubKeyProps.size)
    obj.pubKeyProps.foreach(prop => w.putBytes(prop.pubKeyBytes))
  }

  override def parse(r: Reader): ThresholdCurve25519Proposition = {
    val threshold: Int = r.getUInt().toIntExact

    val numSigs: Int = r.getUInt().toIntExact
    val signatures: Set[PublicKeyCurve25519Proposition] =
      (0 until numSigs).map(_ => PublicKeyCurve25519PropositionSerializer.parse(r)).toSet

    ThresholdCurve25519Proposition(threshold, signatures)
  }
}
