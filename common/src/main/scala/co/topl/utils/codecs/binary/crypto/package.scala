package co.topl.utils.codecs.binary

import co.topl.crypto.PublicKey
import co.topl.crypto.hash.digest.{Digest, Digest32, InvalidDigestFailure}
import co.topl.crypto.signatures.Signature
import co.topl.utils.codecs.binary.valuetypes.codecs.bytesCodec
import scodec.{Attempt, Codec, Err}
import co.topl.crypto.implicits._

import scala.language.implicitConversions

package object crypto {

  trait Codecs {

    def publicKeyCodec(size: Int): Codec[PublicKey] =
      bytesCodec(size)
        .xmap[PublicKey](bytes => PublicKey(bytes), pk => pk.value)
        .as[PublicKey]

    def signatureCodec(size: Int): Codec[Signature] =
      bytesCodec(size)
        .xmap[Signature](bytes => Signature(bytes), signature => signature.value)
        .as[Signature]

    implicit val digest32Codec: Codec[Digest32] =
      bytesCodec(Digest32.size)
        .exmapc(bytes =>
          Digest32.validated(bytes).map(Attempt.successful).valueOr(errs => Attempt.failure(Err(errs.toString)))
        )(digest32 => Attempt.successful(digest32.value))
  }

  trait Instances {
    implicit def digestAsBytes[T: Digest]: AsBytes[Infallible, T] = AsBytes.infallible(_.bytes)

    implicit def digestFromBytes[T: Digest]: FromBytes[InvalidDigestFailure, T] = Digest[T].from

    implicit val publicKeyAsBytes: AsBytes[Infallible, PublicKey] = AsBytes.infallible(_.value)

    implicit val publicKeyFromBytes: FromBytes[Infallible, PublicKey] = FromBytes.infallible(PublicKey(_))
  }

  object codecs extends Codecs

  object implicits extends Instances
}
