package co.topl.codecs.bytes.typeclasses

import com.google.protobuf.ByteString
import scodec.Attempt
import scodec.Encoder
import simulacrum.typeclass

/**
 * Typeclass for encoding a value into its byte representation for signing routines.
 *
 * @tparam T the value that this typeclass is defined for
 */
@typeclass trait Signable[T] {

  /**
   * Gets the byte representation of the value that should be used as the message-to-sign.
   * @param value the value to convert into bytes
   * @return an array of bytes representing the signable data of T
   */
  def signableBytes(value: T): ByteString
}

object Signable {

  def fromScodecEncoder[T: Encoder]: Signable[T] = t =>
    Encoder[T].encode(t) match {
      case Attempt.Successful(value) => ByteString.copyFrom(value.toByteBuffer)
      case Attempt.Failure(cause)    => throw new IllegalArgumentException(cause.messageWithContext)
    }
}
