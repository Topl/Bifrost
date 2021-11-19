package co.topl.nodeCodecs.binary.scodecs.network.message

import cats.implicits._
import co.topl.codecs._
import co.topl.modifier.ModifierId
import co.topl.modifier.NodeViewModifier.ModifierTypeId
import co.topl.network.BifrostSyncInfo
import co.topl.network.message.Messages.MessagesV1._
import co.topl.network.message.{Message, MessageCode, Transmission, TransmissionContent, TransmissionHeader}
import co.topl.network.peer.PeerSpec
import co.topl.nodeCodecs.binary.scodecs.network.peer._
import scodec.codecs.discriminated
import scodec.{Attempt, Codec, Err}
import shapeless.{::, HNil}

import scala.collection.immutable.ListMap

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

  implicit val messageCodeCodec: Codec[MessageCode] = byteCodec.as[MessageCode]

  def transmissionHeaderCodec(magicBytes: Array[Byte]): Codec[TransmissionHeader] =
    (magicBytesCodec(magicBytes) :: messageCodeCodec :: intCodec)
      .xmapc { case _ :: messageCode :: dataLength :: HNil =>
        TransmissionHeader(messageCode, dataLength)
      }(header => magicBytes :: header.code :: header.dataLength :: HNil)

  def transmissionContentCodec(dataLength: Int): Codec[TransmissionContent] =
    (bytesCodec(Transmission.checksumLength) :: bytesCodec(dataLength)).as[TransmissionContent]

  def transmissionCodec(magicBytes: Array[Byte]): Codec[Transmission] =
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

  implicit val bifrostSyncInfoResponseCodec: Codec[BifrostSyncInfoResponse] =
    listCodec[ModifierId]
      .as[Seq[ModifierId]]
      .xmapc(ids => BifrostSyncInfoResponse(BifrostSyncInfo(ids)))(response => response.syncInfo.lastBlockIds)

  implicit val inventoryResponseCodec: Codec[InventoryResponse] =
    (modifierTypeIdCodec :: listCodec[ModifierId].as[Seq[ModifierId]]).as[InventoryResponse]

  implicit val handshakeCodec: Codec[Handshake] =
    (uLongCodec :: peerSpecCodec).xmapc { case time :: peerSpec :: HNil =>
      Handshake(peerSpec, time)
    }(handshake => handshake.time :: handshake.peerSpec :: HNil)

  implicit val peersSpecRequestCodec: Codec[PeersSpecRequest] =
    Codec[Unit].xmap[PeersSpecRequest](_ => PeersSpecRequest(), _ => ())

  implicit val peersSpecResponseCodec: Codec[PeersSpecResponse] =
    listCodec[PeerSpec].as[Seq[PeerSpec]].as[PeersSpecResponse]

  // TODO make a new type called shortList for a list of max size of ushort
  implicit val modifiersRequestCodec: Codec[ModifiersRequest] =
    (modifierTypeIdCodec :: listCodec[ModifierId].as[Seq[ModifierId]]).as[ModifiersRequest]

  implicit val modifiersResponseCodec: Codec[ModifiersResponse] =
    (modifierTypeIdCodec ::
      listMapCodec(modifierIdCodec, listCodec(byteCodec).as[Array[Byte]])
        .xmapc[Map[ModifierId, Array[Byte]]](listMap => listMap)(map => ListMap.from(map)))
      .as[ModifiersResponse]

  /**
   * Message codec expects the message code byte followed by the message data bytes.
   */
  implicit val messageCodec: Codec[Message] =
    discriminated[Message]
      .by(byteCodec)
      .typecase(BifrostSyncInfoResponse.messageCode, bifrostSyncInfoResponseCodec)
      .typecase(InventoryResponse.messageCode, inventoryResponseCodec)
      .typecase(Handshake.messageCode, handshakeCodec)
      .typecase(PeersSpecRequest.messageCode, peersSpecRequestCodec)
      .typecase(PeersSpecResponse.messageCode, peersSpecResponseCodec)
      .typecase(ModifiersRequest.messageCode, modifiersRequestCodec)
      .typecase(ModifiersResponse.messageCode, modifiersResponseCodec)
}
