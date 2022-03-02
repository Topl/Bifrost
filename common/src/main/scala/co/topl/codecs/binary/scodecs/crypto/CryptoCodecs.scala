package co.topl.codecs.binary.scodecs.crypto

import co.topl.codecs.binary.scodecs.valuetypes._
import co.topl.crypto.hash.digest.Digest32
import co.topl.crypto.signatures.Signature
import co.topl.crypto.{PrivateKey, PublicKey}
import scodec.{Attempt, Codec, Err}
import co.topl.codecs.binary.scodecs.valuetypes._

trait CryptoCodecs {

  def publicKeyCodec(size: Int): Codec[PublicKey] =
    bytesCodec(size)
      .xmap[PublicKey](bytes => PublicKey(bytes), pk => pk.value)

  def privateKeyCodec(size: Int): Codec[PrivateKey] =
    bytesCodec(size)
      .xmap[PrivateKey](bytes => PrivateKey(bytes), sk => sk.value)

  def signatureCodec(size: Int): Codec[Signature] =
    bytesCodec(size)
      .xmap[Signature](bytes => Signature(bytes), signature => signature.value)

  implicit val digest32Codec: Codec[Digest32] =
    bytesCodec(Digest32.size)
      .exmapc(bytes =>
        Digest32.validated(bytes).map(Attempt.successful).valueOr(errs => Attempt.failure(Err(errs.toString)))
      )(digest32 => Attempt.successful(digest32.value))
}
