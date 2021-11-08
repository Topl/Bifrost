package co.topl.codecs.binary.scodecs.attestation.keyManagement

import co.topl.attestation.keyManagement.{PrivateKeyCurve25519, PrivateKeyEd25519}
import co.topl.crypto.signatures.{Curve25519, Ed25519}
import co.topl.codecs.binary.scodecs.crypto._
import scodec.Codec

trait KeyManagementCodecs {

  implicit val privateKeyCurve25519Codec: Codec[PrivateKeyCurve25519] =
    (privateKeyCodec(Curve25519.KeyLength) :: publicKeyCodec(Curve25519.KeyLength))
      .as[PrivateKeyCurve25519]

  implicit val privateKeyEd25519Codec: Codec[PrivateKeyEd25519] =
    (privateKeyCodec(Ed25519.KeyLength) :: publicKeyCodec(Ed25519.KeyLength))
      .as[PrivateKeyEd25519]

}
