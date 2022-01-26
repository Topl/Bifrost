package co.topl.network.catsinstances.showinstances

import cats.Show
import cats.implicits._
import co.topl.codecs._
import co.topl.network.message.Messages.MessagesV1
import co.topl.network.message.{Transmission, TransmissionContent, TransmissionHeader}
import co.topl.network.peer.PeerSpec
import co.topl.utils.StringDataTypes.implicits._
import co.topl.utils.catsInstances._

trait ShowInstances {

  implicit val peerSpecShow: Show[PeerSpec] = peerSpec => s"""
          |PeerSpec {
          |  agentName: ${peerSpec.agentName},
          |  version: ${peerSpec.version},
          |  nodeName: ${peerSpec.nodeName},
          |  address: ${peerSpec.declaredAddress},
          |  features: ${peerSpec.features}
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

  implicit val peersSpecRequestShow: Show[MessagesV1.PeersSpecRequest] = _ => s"PeersSpecRequest {}"

  implicit val transmissionHeaderShow: Show[TransmissionHeader] = header =>
    s"TransmissionHeader { code: '${header.code}', data length: ${header.dataLength} }"

  implicit val transmissionContentShow: Show[TransmissionContent] = content =>
    s"TransmissionContent { checksum: ${content.checksum.encodeAsBase16.show}, " +
    s"data: ${content.data.encodeAsBase16.show} }"

  implicit val transmissionShow: Show[Transmission] = transmission =>
    s"Transmission { header: ${transmission.header.show}, content: ${transmission.content.show} }"
}
