package co.topl.utils.codecs.binary

import cats.implicits._
import co.topl.utils.codecs.FromBytes
import simulacrum.typeclass

/**
 * Type-class representing the ability to decode a value of type `A` from a byte-string.
 * @tparam A the value to attempt to decode
 */
@typeclass
trait IterableBytesDecoder[A] {

  /**
   * Attempts to decode a value `A` from an iterable of binary data.
   * @param bytes the bytes to decode the value from
   * @return a result representing the decoded value and remaining bytes if decoding was successful
   */
  def decode(bytes: Iterable[Byte]): DecoderResult[A]
}

object IterableBytesDecoder {

  trait Instances {

    implicit def toFromBytes[T: IterableBytesDecoder]: FromBytes[DecoderFailure, T] =
      IterableBytesDecoder[T].decode(_).map(_._1).toValidatedNec
  }

  trait Implicits extends Instances with ToIterableBytesDecoderOps

  object implicits extends Implicits

}
