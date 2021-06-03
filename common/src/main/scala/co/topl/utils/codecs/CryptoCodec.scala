package co.topl.utils.codecs

import co.topl.crypto.{PrivateKey, PublicKey}
import co.topl.crypto.hash.digest.{Digest, Digest32, InvalidDigestFailure}
import co.topl.crypto.hash.implicits._
import co.topl.crypto.signatures.Signature
import co.topl.utils.encode.Base58
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder}

object CryptoCodec {

  trait AsBytesInstances {
    import AsBytes.infallible

    implicit def digestBytesEncoder[T: Digest]: AsBytes[Infallible, T] = infallible(_.bytes)
    implicit val signatureEncoder: AsBytes[Infallible, Signature] = infallible(_.value)
    implicit val publicKeyEncoder: AsBytes[Infallible, PublicKey] = infallible(_.value)
    implicit val privateKeyEncoder: AsBytes[Infallible, PrivateKey] = infallible(_.value)
  }

  trait FromBytesInstances {
    import FromBytes.infallible

    implicit def digestBytesDecoder[T: Digest]: FromBytes[InvalidDigestFailure, T] = Digest[T].from(_)
    implicit val publicKeyDecoder: FromBytes[Infallible, PublicKey] = infallible(PublicKey(_))
    implicit val privateKeyDecoder: FromBytes[Infallible, PrivateKey] = infallible(PrivateKey(_))
  }

  trait JsonEncoderInstances {
    import implicits.digestBytesEncoder

    implicit val digestJsonEncoder: Encoder[Digest32] = (d: Digest32) => Base58.encode(d).asJson
  }

  trait JsonDecoderInstances {

    implicit val digestJsonDecoder: Decoder[Digest32] =
      Decoder.decodeString.emap(str => Digest32.validated(Base58.decode(str).get).leftMap(x => s"$x").toEither)
  }

  object implicits extends AsBytesInstances with FromBytesInstances with JsonEncoderInstances with JsonDecoderInstances
}
