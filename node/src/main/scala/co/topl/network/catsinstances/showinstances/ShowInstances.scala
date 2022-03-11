package co.topl.network.catsinstances.showinstances

import cats.Show
import cats.implicits._
import co.topl.codecs._
import co.topl.network.message.Messages.MessagesV1
import co.topl.network.message.{Transmission, TransmissionContent, TransmissionHeader}
import co.topl.network.peer.PeerMetadata
import co.topl.utils.implicits._

trait ShowInstances {

  implicit val peerMetadataShow: Show[PeerMetadata] = peerMetadata => s"""
          |PeerMetadata {
          |  agentName: ${peerMetadata.agentName},
          |  version: ${peerMetadata.version},
          |  nodeName: ${peerMetadata.nodeName},
          |  address: ${peerMetadata.declaredAddress},
          |  features: ${peerMetadata.features}
          |}
          |""".stripMargin

  implicit val handshakeShow: Show[MessagesV1.Handshake] = handshake => s"""
          |Handshake {
          |  peerSpec: ${handshake.peerSpec.show},
          |  time: ${handshake.time}
          |}
          |""".stripMargin

  implicit val inventoryResponseShow: Show[MessagesV1.InventoryResponse] = inventory => s"""
       | InventoryResponse {
       |   typeId: ${inventory.typeId},
       |   ids: ${inventory.ids.show}
       | }
       |""".stripMargin

  implicit val peersMetadataRequestShow: Show[MessagesV1.PeersMetadataRequest] = _ => s"PeersMetadataRequest {}"

  implicit val transmissionHeaderShow: Show[TransmissionHeader] = header =>
    s"TransmissionHeader { code: '${header.code}', data length: ${header.dataLength} }"

  implicit val transmissionContentShow: Show[TransmissionContent] = content =>
    s"TransmissionContent { checksum: ${content.checksum.encodeAsBase16.show}, " +
    s"data: ${content.data.encodeAsBase16.show} }"

  implicit val transmissionShow: Show[Transmission] = transmission =>
    s"Transmission { header: ${transmission.header.show}, content: ${transmission.content.show} }"
}
