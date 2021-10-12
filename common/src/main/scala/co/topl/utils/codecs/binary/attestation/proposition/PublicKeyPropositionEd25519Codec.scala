package co.topl.utils.codecs.binary.attestation.proposition

import co.topl.attestation.PublicKeyPropositionEd25519
import co.topl.crypto.signatures.{Curve25519, Ed25519}
import co.topl.utils.codecs.binary.crypto.codecs.publicKey
import scodec.Codec

object PublicKeyPropositionEd25519Codec {

  val codec: Codec[PublicKeyPropositionEd25519] = publicKey(Ed25519.KeyLength).as[PublicKeyPropositionEd25519]

  trait Codecs {
    val publicKeyPropositionEd25519: Codec[PublicKeyPropositionEd25519] = codec
  }

  trait Implicits {
    implicit val implicitPublicKeyPropositionEd25519: Codec[PublicKeyPropositionEd25519] = codec
  }

  object codecs extends Codecs

  object implicits extends Implicits

}
