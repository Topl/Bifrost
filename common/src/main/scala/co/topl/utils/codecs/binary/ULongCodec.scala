package co.topl.utils.codecs.binary

import cats.implicits._
import scala.annotation.tailrec

object ULongCodec {

  /**
   * Recursive function to decode a `ULong` from a set of bytes using the VLQ method.
   *
   * Original Java Source:
   *
   * http://github.com/google/protobuf/blob/a7252bf42df8f0841cf3a0c85fdbf1a5172adecb/java
   * /core/src/main/java/com/google/protobuf/CodedInputStream.java#L2653
   *
   * Faster Java Source
   *
   * http://github.com/google/protobuf/blob/a7252bf42df8f0841cf3a0c85fdbf1a5172adecb/java
   * /core/src/main/java/com/google/protobuf/CodedInputStream.java#L1085
   *
   * @param currentResult the current result as of this step
   * @param shift the current shift to apply to the next byte
   * @param remainingBytes the remaining bytes to parse
   * @param iteration the current iteration of the recursive loop
   * @return if successful, a decoded `ULong` value and the remaining non-decoded bytes
   */
  @tailrec
  private def decodeHelper(
    currentResult:  ULong,
    shift:          Int,
    remainingBytes: Iterable[Byte]
  ): DecoderResult[ULong] =
    (shift, remainingBytes) match {
      // we are done decoding a ULong when the shift value is beyond the index 63
      case (s, _) if s >= 64 => (currentResult, remainingBytes).asRight
      // we are done decoding a ULong if we encounter a 0x80-valued byte
      case (_, head :: tail) if (head & 0x80) == 0 =>
        // decode the next byte into the output ULong value at the slot defined by the shift amount, then complete
        (currentResult | ((head & 0x7f).toLong << shift), tail).asRight
      case (_, head :: tail) =>
        // decode the next byte into the output ULong value and attempt a decode on the next byte with an updated shift
        decodeHelper(currentResult | ((head & 0x7f).toLong << shift), shift + 7, tail)
      // fails because there are no bytes left to decode and we didn't encounter a 0x80 byte
      case _ => DecoderFailure.asLeft
    }

  /**
   * Decodes a `ULong` value from a set of bytes.
   * @param from the collection of bytes to decode a `ULong` value from
   * @return if successful, a decoded `ULong` value and the remaining non-decoded bytes
   */
  def decode(from: Iterable[Byte]): DecoderResult[ULong] =
    decodeHelper(currentResult = 0, shift = 0, remainingBytes = from)

  trait Implicits {
    implicit def uLongDecoder: IterableBytesDecoder[ULong] = decode
  }
}
