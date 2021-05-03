package attestation.serialization

import attestation.{PublicKeyPropositionCurve25519, ThresholdPropositionCurve25519}
import utils.Extensions._
import utils.serialization.{GjalSerializer, Reader, Writer}

/**
 * For serializing a ThresholdProposition
 */
object ThresholdPropositionCurve25519Serializer extends GjalSerializer[ThresholdPropositionCurve25519] {

  override def serialize(obj: ThresholdPropositionCurve25519, w: Writer): Unit = {
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
