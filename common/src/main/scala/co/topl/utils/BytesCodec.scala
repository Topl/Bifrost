package co.topl.utils

import cats.data.ValidatedNec
import cats.implicits._
import co.topl.crypto.hash.digest.{Digest, InvalidDigestError}
import co.topl.crypto.hash.implicits._
import co.topl.crypto.signatures.{PrivateKey, PublicKey, Signature}
import co.topl.modifier.block.BloomFilter.BloomTopic
import io.iohk.iodb.ByteArrayWrapper
import simulacrum.typeclass

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
 * implicit val stringAsBytes: AsBytes[String] = _.getBytes("UTF-8")
 *
 * "Test".encodeAsBytes
 * }}}
 * @tparam Decoded The domain-specific representation
 */
@typeclass
trait AsBytes[Decoded] {
  def encodeAsBytes(decoded: Decoded): Array[Byte]
}

object AsBytes {

  trait Instances {
    implicit val identityBytesEncoder: AsBytes[Array[Byte]] = x => x

    implicit def digestBytesEncoder[T: Digest]: AsBytes[T] = _.bytes

    implicit val signatureEncoder: AsBytes[Signature] = _.value

    implicit val publicKeyEncoder: AsBytes[PublicKey] = _.value

    implicit val privateKeyEncoder: AsBytes[PrivateKey] = _.value

    implicit val bloomTopicEncoder: AsBytes[BloomTopic] = _.value
  }

  object implicits extends Instances with AsBytes.ToAsBytesOps
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
 * bytes.infallibleDecodeTo[ByteArrayWrapper]
 * }}}
 * @tparam Decoded The domain-specific representation
 */
trait FromBytes[DecodeFailure, Decoded] {
  def decode(encoded: Array[Byte]): ValidatedNec[DecodeFailure, Decoded]
}

object FromBytes {

  /** Represents a decode that will never fail. Should never be returned from a decode. */
  case class Infallible()

  class Ops(val instance: Array[Byte]) extends AnyVal {

    def decodeTo[DecodeFailure, Decoded](implicit
      decoder: FromBytes[DecodeFailure, Decoded]
    ): ValidatedNec[DecodeFailure, Decoded] =
      decoder.decode(instance)

    def infallibleDecodeTo[Decoded](implicit decoder: FromBytes[Infallible, Decoded]): Decoded =
      decoder.decode(instance).getOrElse(throw new Exception("Failed to decode infallible value!"))
  }

  trait ToOps {
    implicit def toDecoderOps(target: Array[Byte]): Ops = new Ops(target)

    implicit def toDecoderOps[T: AsBytes](target: T): Ops = new Ops(AsBytes[T].encodeAsBytes(target))
  }

  trait Instances {
    implicit val identityBytesDecoder: FromBytes[Infallible, Array[Byte]] = _.validNec[Infallible]

    implicit def digestBytesDecoder[T: Digest]: FromBytes[InvalidDigestError, T] = Digest[T].from(_)

    implicit def byteArrayWrapperDecoder: FromBytes[Infallible, ByteArrayWrapper] =
      ByteArrayWrapper(_).validNec[Infallible]
  }

  object implicits extends ToOps with Instances
}
