package co.topl.utils

import cats.data.ValidatedNec
import cats.implicits._
import co.topl.crypto.hash.digest.{Digest, InvalidDigestError}
import co.topl.crypto.hash.implicits._
import co.topl.crypto.signatures.{PrivateKey, PublicKey, Signature}
import co.topl.modifier.block.BloomFilter.BloomTopic

import scala.language.implicitConversions

/**
 * A typeclass which encodes a value to a byte array
 *
 * Usage:
 * {{{
 * import cats.implicits._
 * import co.topl.utils.AsBytes.implicits._
 *
 * sealed abstract class StringAsBytesFailure
 * implicit val stringAsBytes: AsBytes[StringAsBytesFailure, String] = _.getBytes("UTF-8").validNec
 *
 * "Test".encodeAsBytes[StringAsBytesFailure]
 * }}}
 * @tparam Decoded The domain-specific representation
 */
trait AsBytes[EncodingFailure, Decoded] {
  def encode(decoded: Decoded): ValidatedNec[EncodingFailure, Array[Byte]]
}

object AsBytes {

  class Ops[T](val instance: T) extends AnyVal {

    def encodeAsBytes[EncodingFailure](implicit
      encoder: AsBytes[EncodingFailure, T]
    ): ValidatedNec[EncodingFailure, Array[Byte]] =
      encoder.encode(instance)
  }

  trait ToOps {
    implicit def toEncoderOps[T](target: T): Ops[T] = new Ops(target)
  }

  trait Instances {
    implicit val identityBytesEncoder: AsBytes[Nothing, Array[Byte]] = _.validNec[Nothing]

    implicit def digestBytesEncoder[T: Digest]: AsBytes[Nothing, T] = _.bytes.validNec[Nothing]

    implicit val signatureEncoder: AsBytes[Nothing, Signature] = _.value.validNec[Nothing]

    implicit val publicKeyEncoder: AsBytes[Nothing, PublicKey] = _.value.validNec[Nothing]

    implicit val privateKeyEncoder: AsBytes[Nothing, PrivateKey] = _.value.validNec[Nothing]

    implicit val bloomTopicEncoder: AsBytes[Nothing, BloomTopic] = _.value.validNec[Nothing]
  }

  object implicits extends ToOps with Instances
}

/**
 * A typeclass which decodes a value from a byte array
 *
 * Usage:
 * {{{
 * import cats.implicits._
 * import co.topl.utils.FromBytes.implicits._
 *
 * sealed abstract class BytesAsStringFailure
 * implicit val bytesAsString: FromBytes[BytesAsStringFailure, String] = new String(_, "UTF-8").validNec
 *
 * val bytes: Array[Byte] = ???
 * bytes.decodeTo[BytesAsStringFailure, String]
 * }}}
 * @tparam Decoded The domain-specific representation
 */
trait FromBytes[DecodeFailure, Decoded] {
  def decode(encoded: Array[Byte]): ValidatedNec[DecodeFailure, Decoded]
}

object FromBytes {

  class Ops(val instance: Array[Byte]) extends AnyVal {

    def decodeTo[DecodeFailure, Decoded](implicit
      decoder: FromBytes[DecodeFailure, Decoded]
    ): ValidatedNec[DecodeFailure, Decoded] =
      decoder.decode(instance)
  }

  trait ToOps {
    implicit def toDecoderOps(target: Array[Byte]): Ops = new Ops(target)
  }

  trait Instances {
    implicit val identityBytesDecoder: FromBytes[Nothing, Array[Byte]] = _.validNec[Nothing]

    implicit def digestBytesDecoder[T: Digest]: FromBytes[InvalidDigestError, T] = Digest[T].from(_)
  }

  object implicits extends ToOps with Instances
}
