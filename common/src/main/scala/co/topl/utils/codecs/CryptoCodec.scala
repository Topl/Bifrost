package co.topl.utils.codecs

import co.topl.crypto.{PrivateKey, PublicKey}
import co.topl.crypto.hash.digest.{Digest, Digest32, Digest64, InvalidDigestFailure}
import co.topl.crypto.hash.implicits._
import co.topl.crypto.signatures.Signature
import co.topl.utils.codecs.StringTypesCodec.implicits._
import co.topl.utils.StringTypes.Base58String
import co.topl.utils.codecs.implicits.toDecoderOps
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder}

object CryptoCodec {

  trait CryptoCodecInstances {
    implicit def digestBytesEncoder[T: Digest]: AsBytes[Infallible, T] = AsBytes.infallible(_.bytes)
    implicit val signatureBytesEncoder: AsBytes[Infallible, Signature] = AsBytes.infallible(_.value)
    implicit val publicKeyBytesEncoder: AsBytes[Infallible, PublicKey] = AsBytes.infallible(_.value)
    implicit val privateKeyBytesEncoder: AsBytes[Infallible, PrivateKey] = AsBytes.infallible(_.value)

    implicit def digestBytesDecoder[T: Digest]: FromBytes[InvalidDigestFailure, T] = Digest[T].from(_)
    implicit val publicKeyBytesDecoder: FromBytes[Infallible, PublicKey] = FromBytes.infallible(PublicKey(_))
    implicit val privateKeyBytesDecoder: FromBytes[Infallible, PrivateKey] = FromBytes.infallible(PrivateKey(_))

    implicit val digestJsonEncoder: Encoder[Digest32] = (d: Digest32) => d.infalliblyDecodeTo[Base58String].asJson

    implicit val digestJsonDecoder: Decoder[Digest32] =
      base58JsonDecoder.emap(
        _.decodeTo[InvalidDigestFailure, Digest32].leftMap(err => s"Invalid digest: $err").toEither
      )
  }

  object implicits extends CryptoCodecInstances
}
