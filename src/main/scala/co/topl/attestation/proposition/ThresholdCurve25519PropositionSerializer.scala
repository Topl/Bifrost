package co.topl.attestation.proposition

import co.topl.utils.Extensions._
import co.topl.utils.serialization.{BifrostSerializer, Reader, Writer}
import scorex.crypto.signatures.{Curve25519, PublicKey}

object ThresholdCurve25519PropositionSerializer extends BifrostSerializer[ThresholdCurve25519Proposition] {

  override def serialize(obj: ThresholdCurve25519Proposition, w: Writer): Unit = {
    /* m: Int */
    w.putUInt(obj.threshold)

    /* setOfPubKeyBytes: Set[Array[Byte]] */
    w.putUInt(obj.pubKeyProps.size)
    obj.pubKeyProps.foreach(prop => w.putBytes(prop.pubKeyBytes))
  }

  override def parse(r: Reader): ThresholdCurve25519Proposition = {
    val m: Int = r.getUInt().toIntExact
    val n: Int = r.getUInt().toIntExact
    val setOfPubKeyBytes: Set[PublicKey] = (0 until n).map(_ => PublicKey @@ r.getBytes(Curve25519.KeyLength)).toSet

    ThresholdCurve25519Proposition(m, setOfPubKeyBytes)
  }
}
