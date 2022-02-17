package co.topl.codecs.binary.scodecs.attestation.keyManagement

import co.topl.attestation.keyManagement.{PrivateKeyCurve25519, PrivateKeyEd25519}
import co.topl.codecs.binary.scodecs.crypto._
import co.topl.crypto.signing.{Curve25519, Ed25519}
import scodec.Codec

trait KeyManagementCodecs {

  implicit val privateKeyCurve25519Codec: Codec[PrivateKeyCurve25519] =
    (privateKeyCodec(Curve25519.instance.KeyLength) :: publicKeyCodec(Curve25519.instance.KeyLength))
      .as[PrivateKeyCurve25519]

  implicit val privateKeyEd25519Codec: Codec[PrivateKeyEd25519] =
    (privateKeyCodec(Ed25519.instance.KeyLength) :: publicKeyCodec(Ed25519.instance.KeyLength))
      .as[PrivateKeyEd25519]

}
