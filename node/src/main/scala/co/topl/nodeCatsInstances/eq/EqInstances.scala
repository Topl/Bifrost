package co.topl.nodeCatsInstances.eq

import cats.implicits._
import cats.Eq
import co.topl.modifier.NodeViewModifier.ModifierTypeId
import co.topl.network.message.Messages.MessagesV1
import co.topl.network.message.{Transmission, TransmissionContent, TransmissionHeader}
import co.topl.network.peer.PeerSpec
import co.topl.utils.catsInstances._

import java.net.InetSocketAddress

trait EqInstances {

  implicit val inetSocketAddressEq: Eq[InetSocketAddress] = Eq.fromUniversalEquals

  implicit val peerSpecEq: Eq[PeerSpec] = (p1, p2) =>
    p1.agentName === p2.agentName &&
    p1.version.firstDigit === p1.version.firstDigit &&
    p1.version.secondDigit === p2.version.secondDigit &&
    p1.version.thirdDigit === p2.version.thirdDigit &&
    p1.nodeName === p2.nodeName &&
    p1.declaredAddress === p2.declaredAddress

  implicit val handshakeEq: Eq[MessagesV1.Handshake] = (h1, h2) =>
    h1.time === h2.time &&
    h1.peerSpec === h2.peerSpec

  implicit val modifierTypeIdEq: Eq[ModifierTypeId] = (m1, m2) => m1.value === m2.value

  implicit val inventoryResponseEq: Eq[MessagesV1.InventoryResponse] = (i1, i2) =>
    i1.typeId === i2.typeId &&
    i1.ids === i2.ids

  implicit val transmissionHeaderEq: Eq[TransmissionHeader] = (h1, h2) =>
    h1.code === h2.code && h1.dataLength === h2.dataLength

  implicit val transmissionContentEq: Eq[TransmissionContent] = (c1, c2) =>
    c1.checksum === c2.checksum && c1.data === c2.data

  implicit val transmissionEq: Eq[Transmission] = (t1, t2) => t1.header === t2.header && t1.content === t2.content
}
