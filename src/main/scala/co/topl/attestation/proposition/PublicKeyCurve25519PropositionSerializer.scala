package co.topl.attestation.proposition

import co.topl.utils.serialization.{BifrostSerializer, Reader, Writer}
import scorex.crypto.signatures.{Curve25519, PublicKey}

object PublicKeyCurve25519PropositionSerializer extends BifrostSerializer[PublicKeyCurve25519Proposition] {

  override def serialize(obj: PublicKeyCurve25519Proposition, w: Writer): Unit = {
    w.putBytes(obj.pubKeyBytes)
  }

  override def parse(r: Reader): PublicKeyCurve25519Proposition = {
    PublicKeyCurve25519Proposition(PublicKey @@ r.getBytes(Curve25519.KeyLength))
  }
}
