package co.topl.networking.blockchain

import cats.implicits._
import co.topl.codecs.bytes.scodecs.valuetypes.emptyCodec
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.Transmittable
import co.topl.models.{BlockBodyV2, BlockHeaderV2, Transaction, TypedIdentifier}
import co.topl.networking.multiplexer.MultiplexerCodecBuilder
import co.topl.networking.blockchain.NetworkTypeTags._
import co.topl.networking.typedprotocols.TypedProtocol
import scodec.{Attempt, Codec, DecodeResult, Decoder, Encoder, Err}
import scodec.bits.BitVector

object BlockchainMultiplexerCodecs {

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

  val multiplexerCodec =
    MultiplexerCodecBuilder(Map.empty, Map.empty)
      .withCodec[TypedProtocol.CommonMessages.Start.type](1: Byte)
      .withCodec[TypedProtocol.CommonMessages.Done.type](2: Byte)
      .withCodec[TypedProtocol.CommonMessages.Get[TypedIdentifier]](3: Byte)
      //      .withCodec[TypedProtocol.CommonMessages.Response[SlotData]](4: Byte)
      .withCodec[TypedProtocol.CommonMessages.Response[BlockHeaderV2]](5: Byte)
      .withCodec[TypedProtocol.CommonMessages.Response[BlockBodyV2]](6: Byte)
      .withCodec[TypedProtocol.CommonMessages.Response[Transaction]](7: Byte)
      .withCodec[TypedProtocol.CommonMessages.Push[TypedIdentifier]](8: Byte)
      .multiplexerCodec

}
