package co.topl.network

import co.topl.modifier.ModifierId
import co.topl.modifier.NodeViewModifier.ModifierTypeId
import co.topl.network.peer.PeerMetadata
import co.topl.settings.Version

package object message {

  type MessageCode = Byte

  /** Enumeration of app p2p messages in the network */
  sealed trait Message {

    /**
     * The message version.
     */
    val version: Version

    /**
     * The unique and identifiable message code.
     */
    val messageCode: MessageCode
  }

  object Messages {

    /** Original P2P messages */
    sealed trait MessageV1 extends Message {
      override val version: Version = MessageV1.version
    }

    object MessageV1 {
      val version: Version = Version.initial
    }

    object MessagesV1 {

      /**
       * Requests an [[InventoryResponse]] message that provides modifier ids
       * required by the sender to synchronize their blockchain with the recipient.
       * It allows a peer which has been disconnected or started for the first
       * time to get the data it needs to request the blocks it hasn't seen.
       */
      final case class BifrostSyncInfoRequest(syncInfo: BifrostSyncInfo) extends MessageV1 {
        override val messageCode: MessageCode = BifrostSyncInfoRequest.messageCode
      }

      object BifrostSyncInfoRequest {
        val messageCode: MessageCode = 65: Byte
      }

      /**
       * Transmits one or more inventories of objects known to the transmitting peer.
       * It can be sent unsolicited to announce new transactions or blocks,
       * or it can be sent in reply to a [[BifrostSyncInfoRequest]] message
       * (or application-specific messages like `GetMempool`).
       */
      final case class InventoryResponse(typeId: ModifierTypeId, ids: Seq[ModifierId]) extends MessageV1 {
        override val messageCode: MessageCode = InventoryResponse.messageCode
      }

      object InventoryResponse {
        val messageCode: MessageCode = 55: Byte
      }

      final case class Handshake(peerSpec: PeerMetadata, time: Long) extends MessageV1 {
        override val messageCode: MessageCode = Handshake.messageCode
      }

      object Handshake {
        val messageCode: MessageCode = 75: Byte
      }

      /**
       * Requests a [[PeersMetadataResponse]] message from the receiving node,
       * preferably one with lots of `PeerSpec` of other receiving nodes.
       * The transmitting node can use those `PeerSpec` addresses to quickly update
       * its database of available nodes rather than waiting for unsolicited `Peers`
       * messages to arrive over time.
       */
      final case class PeersMetadataRequest() extends MessageV1 {
        override val messageCode: MessageCode = PeersMetadataRequest.messageCode
      }

      object PeersMetadataRequest {
        val messageCode: MessageCode = 1: Byte
      }

      /**
       * A reply to a [[PeersMetadataRequest]] message and relays connection information about peers
       * on the network.
       */
      final case class PeersMetadataResponse(peers: Seq[PeerMetadata]) extends MessageV1 {
        override val messageCode: MessageCode = PeersMetadataResponse.messageCode
      }

      object PeersMetadataResponse {
        val messageCode: MessageCode = 2: Byte
      }

      /**
       * Requests one or more modifiers from another node.
       * The objects are requested by an inventory, which the requesting node
       * typically received previously by way of an [[InventoryResponse]] message.
       *
       * This message cannot be used to request arbitrary data, such as historic transactions no
       * longer in the memory pool. Full nodes may not even be able to provide older blocks if
       * they’ve pruned old transactions from their block database.
       * For this reason, the [[ModifiersRequest]] message should usually only be used to request
       * data from a node which previously advertised it had that data by sending an [[InventoryResponse]] message.
       */
      final case class ModifiersRequest(typeId: ModifierTypeId, ids: Seq[ModifierId]) extends MessageV1 {
        override val messageCode: MessageCode = ModifiersRequest.messageCode
      }

      object ModifiersRequest {
        val messageCode: MessageCode = 22: Byte
      }

      /**
       * A reply to a [[ModifiersRequest]] message which requested these modifiers.
       */
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
