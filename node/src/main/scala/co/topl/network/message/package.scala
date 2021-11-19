package co.topl.network

import co.topl.modifier.ModifierId
import co.topl.modifier.NodeViewModifier.ModifierTypeId
import co.topl.network.peer.PeerSpec
import co.topl.settings.Version

package object message {

  type MessageCode = Byte

  sealed trait Message {
    val version: Version
    val messageCode: MessageCode
  }

  object Messages {

    sealed trait MessageV1 extends Message {
      override val version: Version = MessageV1.version
    }

    object MessageV1 {
      val version: Version = Version.initial
    }

    object MessagesV1 {

      final case class BifrostSyncInfoResponse(syncInfo: BifrostSyncInfo) extends MessageV1 {
        override val messageCode: MessageCode = BifrostSyncInfoResponse.messageCode
      }

      object BifrostSyncInfoResponse {
        val messageCode: MessageCode = 1: Byte
      }

      final case class InventoryResponse(typeId: ModifierTypeId, ids: Seq[ModifierId]) extends MessageV1 {
        override val messageCode: MessageCode = InventoryResponse.messageCode
      }

      object InventoryResponse {
        val messageCode: MessageCode = 55: Byte
      }

      final case class Handshake(peerSpec: PeerSpec, time: Long) extends MessageV1 {
        override val messageCode: MessageCode = Handshake.messageCode
      }

      object Handshake {
        val messageCode: MessageCode = 75: Byte
      }

      final case class PeersSpecRequest() extends MessageV1 {
        override val messageCode: MessageCode = PeersSpecRequest.messageCode
      }

      object PeersSpecRequest {
        val messageCode: MessageCode = 1: Byte
      }

      final case class PeersSpecResponse(peers: Seq[PeerSpec]) extends MessageV1 {
        override val messageCode: MessageCode = PeersSpecResponse.messageCode
      }

      object PeersSpecResponse {
        val messageCode: MessageCode = 2: Byte
      }

      final case class ModifiersRequest(typeId: ModifierTypeId, ids: Seq[ModifierId]) extends MessageV1 {
        override val messageCode: MessageCode = ModifiersRequest.messageCode
      }

      object ModifiersRequest {
        val messageCode: MessageCode = 22: Byte
      }

      final case class ModifiersResponse(typeId: ModifierTypeId, modifiers: Map[ModifierId, Array[Byte]])
          extends MessageV1 {
        override val messageCode: MessageCode = ModifiersResponse.messageCode
      }

      object ModifiersResponse {
        val messageCode: MessageCode = 33: Byte
      }
    }
  }
}
