package co.topl.networking.multiplexer

import cats.implicits._
import co.topl.codecs.bytes.typeclasses.Transmittable
import co.topl.networking.NetworkTypeTag
import scodec.bits.ByteVector

/**
 * Encodes and decodes ByteVector data across many types.  In other words, this encodes and decodes _all_ messages
 * of the Multiplexer.  The messages are meant to be discriminated by a Byte identifier which is used when selecting
 * the proper sub-encoder or sub-decoder.
 *
 * This codec is effectively type-unsafe, but it achieves pseudo-type-safety through byte discriminators.
 */
trait MultiplexerCodec {
  def decode(prefix:                  Byte)(data: ByteVector): Either[MultiplexerCodecFailure, (Any, NetworkTypeTag[_])]
  def encode[T: NetworkTypeTag](data: T): Either[MultiplexerCodecFailure, (Byte, ByteVector)]
}

/**
 * A type-safe helper for constructing non-type-safe MultiplexerCodecs.
 */
case class MultiplexerCodecBuilder(
  private val decoders: Map[Byte, ByteVector => Either[MultiplexerCodecFailure, (Any, NetworkTypeTag[_])]],
  private val encoders: Map[NetworkTypeTag[_], Any => Either[MultiplexerCodecFailure, (Byte, ByteVector)]]
) {

  /**
   * Include a codec for the given message type T under the given `prefix` Byte.  The value is expected to have
   * a corresponding `NetworkTypeTag` and `Transmittable` typeclass.
   */
  def withCodec[T: NetworkTypeTag: Transmittable](prefix: Byte): MultiplexerCodecBuilder =
    copy(
      decoders = decoders.updated(
        prefix,
        data =>
          Transmittable[T]
            .fromTransmittableBytes(data)
            .leftMap(MultiplexerCodecFailures.DecodeFailure(_): MultiplexerCodecFailure)
            .map(_ -> implicitly[NetworkTypeTag[T]])
      ),
      encoders = encoders.updated(
        implicitly[NetworkTypeTag[T]],
        value => (prefix, Transmittable[T].transmittableBytes(value.asInstanceOf[T])).asRight
      )
    )

  /**
   * Resolves this builder into a type-unsafe MultiplexerCodec
   */
  def multiplexerCodec: MultiplexerCodec =
    new MultiplexerCodec {

      def decode(prefix: Byte)(data: ByteVector): Either[MultiplexerCodecFailure, (Any, NetworkTypeTag[_])] =
        decoders
          .get(prefix)
          .toRight(MultiplexerCodecFailures.UnknownBytePrefix(prefix): MultiplexerCodecFailure)
          .flatMap(_.apply(data))

      def encode[T: NetworkTypeTag](data: T): Either[MultiplexerCodecFailure, (Byte, ByteVector)] =
        encoders
          .get(implicitly[NetworkTypeTag[T]])
          .toRight(MultiplexerCodecFailures.UnknownData(data): MultiplexerCodecFailure)
          .flatMap(_.apply(data))
    }
}

sealed abstract class MultiplexerCodecFailure

object MultiplexerCodecFailures {
  case class UnknownBytePrefix(byte: Byte) extends MultiplexerCodecFailure
  case class UnknownData[T](data: T) extends MultiplexerCodecFailure
  case class DecodeFailure(message: String) extends MultiplexerCodecFailure
}
