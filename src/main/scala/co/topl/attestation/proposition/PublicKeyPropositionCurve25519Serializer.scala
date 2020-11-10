package co.topl.attestation.proposition

import co.topl.utils.serialization.{BifrostSerializer, Reader, Writer}
import scorex.crypto.signatures.{Curve25519, PublicKey}

object PublicKeyPropositionCurve25519Serializer extends BifrostSerializer[PublicKeyPropositionCurve25519] {

  override def serialize( obj: PublicKeyPropositionCurve25519, w: Writer): Unit = {
    w.putBytes(obj.pubKeyBytes)
  }

  override def parse(r: Reader): PublicKeyPropositionCurve25519 = {
    PublicKeyPropositionCurve25519(PublicKey @@ r.getBytes(Curve25519.KeyLength))
  }
}
