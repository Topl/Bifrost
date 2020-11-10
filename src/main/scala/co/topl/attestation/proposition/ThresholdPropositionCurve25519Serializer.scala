package co.topl.attestation.proposition

import co.topl.attestation.proof.{ SignatureCurve25519, SignatureCurve25519Serializer }
import co.topl.utils.Extensions._
import co.topl.utils.serialization.{ BifrostSerializer, Reader, Writer }
import scorex.crypto.signatures.{ Curve25519, PublicKey }

object ThresholdPropositionCurve25519Serializer extends BifrostSerializer[ThresholdPropositionCurve25519] {

  override def serialize( obj: ThresholdPropositionCurve25519, w: Writer): Unit = {
    /* m: Int */
    w.putUInt(obj.threshold)

    /* setOfPubKeyBytes: Set[Array[Byte]] */
    w.putUInt(obj.pubKeyProps.size)
    obj.pubKeyProps.foreach(prop => w.putBytes(prop.pubKeyBytes))
  }

  override def parse(r: Reader): ThresholdPropositionCurve25519 = {
    val threshold: Int = r.getUInt().toIntExact

    val numSigs: Int = r.getUInt().toIntExact
    val signatures: Set[PublicKeyPropositionCurve25519] =
      (0 until numSigs).map(_ => PublicKeyPropositionCurve25519Serializer.parse(r)).toSet

    ThresholdPropositionCurve25519(threshold, signatures)
  }
}
