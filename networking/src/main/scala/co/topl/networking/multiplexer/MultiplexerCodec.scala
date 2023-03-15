package co.topl.networking.multiplexer

import cats.implicits._
import co.topl.codecs.bytes.scodecs.valuetypes.emptyCodec
import co.topl.codecs.bytes.typeclasses.Transmittable
import co.topl.networking.NetworkTypeTag
import co.topl.networking.typedprotocols.TypedProtocol
import com.google.protobuf.ByteString

/**
 * Encodes and decodes ByteVector data across many types.  In other words, this encodes and decodes _all_ messages
 * of the Multiplexer.  The messages are meant to be discriminated by a Byte identifier which is used when selecting
 * the proper sub-encoder or sub-decoder.
 *
 * This codec is effectively type-unsafe, but it achieves pseudo-type-safety through byte discriminators.
 */
trait MultiplexerCodec {
  def decode(prefix:                  Byte)(data: ByteString): Either[MultiplexerCodecFailure, (Any, NetworkTypeTag[_])]
  def encode[T: NetworkTypeTag](data: T): Either[MultiplexerCodecFailure, (Byte, ByteString)]
}

/**
 * A type-safe helper for constructing non-type-safe MultiplexerCodecs.
 */
case class MultiplexerCodecBuilder(
  private val decoders: Map[Byte, ByteString => Either[MultiplexerCodecFailure, (Any, NetworkTypeTag[_])]],
  private val encoders: Map[NetworkTypeTag[_], Any => Either[MultiplexerCodecFailure, (Byte, ByteString)]]
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

      def decode(prefix: Byte)(data: ByteString): Either[MultiplexerCodecFailure, (Any, NetworkTypeTag[_])] =
        decoders
          .get(prefix)
          .toRight(MultiplexerCodecFailures.UnknownBytePrefix(prefix): MultiplexerCodecFailure)
          .flatMap(_.apply(data))

      def encode[T: NetworkTypeTag](data: T): Either[MultiplexerCodecFailure, (Byte, ByteString)] =
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
    new Transmittable[TypedProtocol.CommonMessages.Get[Query]] {

      def transmittableBytes(value: TypedProtocol.CommonMessages.Get[Query]): ByteString =
        Transmittable[Query].transmittableBytes(value.query)

      def fromTransmittableBytes(bytes: ByteString): Either[String, TypedProtocol.CommonMessages.Get[Query]] =
        Transmittable[Query].fromTransmittableBytes(bytes).map(TypedProtocol.CommonMessages.Get(_))

    }

  implicit def commonMessagesResponseTransmittable[T: Transmittable]
    : Transmittable[TypedProtocol.CommonMessages.Response[T]] =
    new Transmittable[TypedProtocol.CommonMessages.Response[T]] {

      def transmittableBytes(value: TypedProtocol.CommonMessages.Response[T]): ByteString =
        value.dataOpt.fold(
          ByteString.copyFrom(Array[Byte](0))
        )(data =>
          ByteString
            .copyFrom(Array[Byte](1))
            .concat(Transmittable[T].transmittableBytes(data))
        )

      def fromTransmittableBytes(bytes: ByteString): Either[String, TypedProtocol.CommonMessages.Response[T]] =
        bytes.byteAt(0) match {
          case 0 =>
            TypedProtocol.CommonMessages.Response(none[T]).asRight
          case 1 =>
            Transmittable[T]
              .fromTransmittableBytes(bytes.substring(1))
              .map(_.some)
              .map(TypedProtocol.CommonMessages.Response(_))
          case _ =>
            Left("Invalid byte prefix for Response")
        }

    }

  implicit def commonMessagesPushTransmittable[T: Transmittable]: Transmittable[TypedProtocol.CommonMessages.Push[T]] =
    new Transmittable[TypedProtocol.CommonMessages.Push[T]] {

      def transmittableBytes(value: TypedProtocol.CommonMessages.Push[T]): ByteString =
        Transmittable[T].transmittableBytes(value.data)

      def fromTransmittableBytes(bytes: ByteString): Either[String, TypedProtocol.CommonMessages.Push[T]] =
        Transmittable[T].fromTransmittableBytes(bytes).map(TypedProtocol.CommonMessages.Push(_))

    }

  implicit val commonMessagesDoneTransmittable: Transmittable[TypedProtocol.CommonMessages.Done.type] =
    Transmittable.instanceFromCodec(emptyCodec(TypedProtocol.CommonMessages.Done))
}

sealed abstract class MultiplexerCodecFailure

object MultiplexerCodecFailures {
  case class UnknownBytePrefix(byte: Byte) extends MultiplexerCodecFailure
  case class UnknownData[T](data: T) extends MultiplexerCodecFailure
  case class DecodeFailure(message: String) extends MultiplexerCodecFailure
}
