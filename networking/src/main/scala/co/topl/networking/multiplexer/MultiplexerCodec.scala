package co.topl.networking.multiplexer

import cats.implicits._
import co.topl.codecs.bytes.scodecs.valuetypes.emptyCodec
import co.topl.codecs.bytes.typeclasses.Transmittable
import co.topl.networking.NetworkTypeTag
import co.topl.networking.typedprotocols.TypedProtocol
import scodec.bits.{BitVector, ByteVector}
import scodec.{Attempt, Codec, DecodeResult, Decoder, Encoder, Err}

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

object MultiplexerCodecBuilder {

  def apply(): MultiplexerCodecBuilder =
    MultiplexerCodecBuilder(Map.empty, Map.empty)
}

object MultiplexerCodecs {

  implicit val commonMessagesStartTransmittable: Transmittable[TypedProtocol.CommonMessages.Start.type] =
    Transmittable.instanceFromCodec(emptyCodec(TypedProtocol.CommonMessages.Start))

  implicit def commonMessagesGetTransmittable[Query: Transmittable]
    : Transmittable[TypedProtocol.CommonMessages.Get[Query]] =
    Transmittable.instanceFromCodec[TypedProtocol.CommonMessages.Get[Query]](
      Codec[TypedProtocol.CommonMessages.Get[Query]](
        (p: TypedProtocol.CommonMessages.Get[Query]) =>
          Attempt.successful(Transmittable[Query].transmittableBytes(p.query)).map(_.toBitVector),
        (p: BitVector) =>
          Attempt.fromEither(
            Transmittable[Query]
              .fromTransmittableBytes(p.toByteVector)
              .leftMap(Err(_))
              .map(DecodeResult(_, BitVector.empty).map(t => TypedProtocol.CommonMessages.Get(t)))
          )
      )
    )

  implicit def commonMessagesResponseTransmittable[T: Transmittable]
    : Transmittable[TypedProtocol.CommonMessages.Response[T]] =
    Transmittable.instanceFromCodec[TypedProtocol.CommonMessages.Response[T]](
      Codec[TypedProtocol.CommonMessages.Response[T]](
        encoder = Encoder((p: TypedProtocol.CommonMessages.Response[T]) =>
          p.dataOpt match {
            case Some(data) =>
              Attempt
                .successful(BitVector.one ++ Transmittable[T].transmittableBytes(data).toBitVector)
            case _ =>
              Attempt.successful(BitVector.zero)
          }
        ),
        decoder = Decoder((p: BitVector) =>
          (
            if (p.head)
              Attempt.fromEither(
                Transmittable[T]
                  .fromTransmittableBytes(p.tail.toByteVector)
                  .leftMap(Err(_))
                  .map(DecodeResult(_, BitVector.empty).map(t => TypedProtocol.CommonMessages.Response(t.some)))
              )
            else
              Attempt.successful(DecodeResult(TypedProtocol.CommonMessages.Response(none[T]), p.tail))
          )
        )
      )
    )

  implicit def commonMessagesPushTransmittable[T: Transmittable]: Transmittable[TypedProtocol.CommonMessages.Push[T]] =
    Transmittable.instanceFromCodec[TypedProtocol.CommonMessages.Push[T]](
      Codec[TypedProtocol.CommonMessages.Push[T]](
        (p: TypedProtocol.CommonMessages.Push[T]) =>
          Attempt.successful(Transmittable[T].transmittableBytes(p.data)).map(_.toBitVector),
        (p: BitVector) =>
          Attempt.fromEither(
            Transmittable[T]
              .fromTransmittableBytes(p.toByteVector)
              .leftMap(Err(_))
              .map(DecodeResult(_, BitVector.empty).map(TypedProtocol.CommonMessages.Push[T](_)))
          )
      )
    )

  implicit val commonMessagesDoneTransmittable: Transmittable[TypedProtocol.CommonMessages.Done.type] =
    Transmittable.instanceFromCodec(emptyCodec(TypedProtocol.CommonMessages.Done))
}

sealed abstract class MultiplexerCodecFailure

object MultiplexerCodecFailures {
  case class UnknownBytePrefix(byte: Byte) extends MultiplexerCodecFailure
  case class UnknownData[T](data: T) extends MultiplexerCodecFailure
  case class DecodeFailure(message: String) extends MultiplexerCodecFailure
}
