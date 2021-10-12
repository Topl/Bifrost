package co.topl.utils.codecs.binary.attestation.proposition

import co.topl.attestation.PublicKeyPropositionCurve25519
import co.topl.crypto.signatures.Curve25519
import scodec.Codec
import co.topl.utils.codecs.binary.crypto.codecs._

object PublicKeyPropositionCurve25519Codec {

  val codec: Codec[PublicKeyPropositionCurve25519] = publicKey(Curve25519.KeyLength).as[PublicKeyPropositionCurve25519]

  trait Codecs {
    val publicKeyPropositionCurve25519: Codec[PublicKeyPropositionCurve25519] = codec
  }

  trait Implicits {
    implicit val implicitPublicKeyPropositionCurve25519: Codec[PublicKeyPropositionCurve25519] = codec
  }

  object codecs extends Codecs

  object implicits extends Implicits

}
