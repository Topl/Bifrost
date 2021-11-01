package co.topl.utils.codecs.binary.attestation

import co.topl.attestation.keyManagement.{PrivateKeyCurve25519, PrivateKeyEd25519}
import co.topl.crypto.signatures.{Curve25519, Ed25519}
import co.topl.utils.codecs.binary.crypto.codecs._
import scodec.Codec

package object keyManagement {

  trait Codecs {

    implicit val privateKeyCurve25519Codec: Codec[PrivateKeyCurve25519] =
      (privateKeyCodec(Curve25519.KeyLength) :: publicKeyCodec(Curve25519.KeyLength))
        .as[PrivateKeyCurve25519]

    implicit val privateKeyEd25519Codec: Codec[PrivateKeyEd25519] =
      (privateKeyCodec(Ed25519.KeyLength) :: publicKeyCodec(Ed25519.KeyLength))
        .as[PrivateKeyEd25519]

  }

  object codecs extends Codecs
}
