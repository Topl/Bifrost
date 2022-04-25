package co.topl.network.codecs.scodecs.network.message

import cats.implicits._
import co.topl.codecs._
import co.topl.modifier.ModifierId
import co.topl.modifier.NodeViewModifier.ModifierTypeId
import co.topl.network.BifrostSyncInfo
import co.topl.network.message.Messages.MessagesV1._
import co.topl.network.message._
import co.topl.network.peer.PeerMetadata
import co.topl.network.codecs.scodecs.network.peer._
import scodec.codecs.{discriminated, int32}
import scodec.{Attempt, Codec, Err}
import shapeless.{::, HNil}

import scala.reflect.ClassTag

trait MessageCodecs {

  implicit val modifierTypeIdCodec: Codec[ModifierTypeId] =
    byteCodec.xmapc(byte => ModifierTypeId(byte))(mti => mti.value)

  private def magicBytesCodec(magic: Array[Byte]): Codec[Array[Byte]] =
    bytesCodec(Transmission.magicLength)
      .exmapc[Array[Byte]](bytes =>
        Attempt
          .guard(
            java.util.Arrays.equals(magic, bytes),
            s"Wrong magic bytes, expected ${magic.mkString}, got ${bytes.mkString}"
          )
          .map(_ => bytes)
      )(_ => Attempt.successful(magic))

  val messageCodeCodec: Codec[MessageCode] = byteCodec.as[MessageCode]

  def transmissionHeaderCodec(magicBytes: Array[Byte]): Codec[TransmissionHeader] =
    // use Scodec's int32 implementation for a fixed-length integer
    (magicBytesCodec(magicBytes) :: messageCodeCodec :: int32)
      .xmapc { case _ :: messageCode :: dataLength :: HNil =>
        TransmissionHeader(messageCode, dataLength)
      }(header => magicBytes :: header.code :: header.dataLength :: HNil)

  def transmissionContentCodec(dataLength: Int): Codec[TransmissionContent] =
    (bytesCodec(Transmission.checksumLength) :: bytesCodec(dataLength)).as[TransmissionContent]

  def transmissionCodec(magicBytes: Array[Byte]): Codec[Transmission] =
    // only encode the transmission content when the transmission data is not an empty array
    transmissionHeaderCodec(magicBytes).consume[Transmission](header =>
      if (header.dataLength === 0) Codec[Unit].xmap[Transmission](_ => Transmission(header, None), _ => ())
      else
        transmissionContentCodec(header.dataLength)
          .exmapc(content => Attempt.successful(Transmission(header, content.some)))(transmission =>
            transmission.content
              .map(Attempt.successful)
              // case when data length of header is non-zero but content is None
              .getOrElse(Attempt.failure(Err("transmission content must not be empty when data length is non-zero")))
          )
    )(transmission => transmission.header)

  implicit val bifrostSyncInfoResponseCodec: Codec[BifrostSyncInfoRequest] =
    seqCodec[ModifierId]
      .xmapc(ids => BifrostSyncInfoRequest(BifrostSyncInfo(ids)))(response => response.syncInfo.lastBlockIds)

  implicit val inventoryResponseCodec: Codec[InventoryResponse] =
    (modifierTypeIdCodec :: seqCodec[ModifierId]).as[InventoryResponse]

  implicit val handshakeCodec: Codec[Handshake] =
    (uLongCodec :: peerMetadataCodec).xmapc { case time :: peerSpec :: HNil =>
      Handshake(peerSpec, time)
    }(handshake => handshake.time :: handshake.peerSpec :: HNil)

  implicit val peersMetadataRequestCodec: Codec[PeersMetadataRequest] =
    Codec[Unit].xmap[PeersMetadataRequest](_ => PeersMetadataRequest(), _ => ())

  implicit val peersMetadataResponseCodec: Codec[PeersMetadataResponse] =
    seqCodec[PeerMetadata].as[PeersMetadataResponse]

  implicit val modifiersRequestCodec: Codec[ModifiersRequest] =
    (modifierTypeIdCodec :: seqCodec[ModifierId]).as[ModifiersRequest]

  implicit val modifiersResponseCodec: Codec[ModifiersResponse] =
    (modifierTypeIdCodec ::
      mapCodec(modifierIdCodec, arrayCodec[Byte]))
      .as[ModifiersResponse]

  /**
   * Message codec expects the message code byte followed by the message data bytes.
   */
  implicit val messageCodec: Codec[Message] =
    discriminated[Message]
      .by(byteCodec)
      .typecase(BifrostSyncInfoRequest.messageCode, bifrostSyncInfoResponseCodec)
      .typecase(InventoryResponse.messageCode, inventoryResponseCodec)
      .typecase(Handshake.messageCode, handshakeCodec)
      .typecase(PeersMetadataRequest.messageCode, peersMetadataRequestCodec)
      .typecase(PeersMetadataResponse.messageCode, peersMetadataResponseCodec)
      .typecase(ModifiersRequest.messageCode, modifiersRequestCodec)
      .typecase(ModifiersResponse.messageCode, modifiersResponseCodec)
}
