package co.topl.networking.blockchain

import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.consensus.models.{BlockHeader, BlockId, SlotData}
import co.topl.networking.NetworkTypeTag
import co.topl.networking.typedprotocols.TypedProtocol
import co.topl.node.models._

object NetworkTypeTags {

  implicit val commonStatesNoneNetworkTypeTag: NetworkTypeTag[TypedProtocol.CommonStates.None.type] =
    NetworkTypeTag.create("TypedProtocol.CommonStates.None")

  implicit val commonStatesIdleNetworkTypeTag: NetworkTypeTag[TypedProtocol.CommonStates.Idle.type] =
    NetworkTypeTag.create("TypedProtocol.CommonStates.Idle")

  implicit val commonStatesBusyNetworkTypeTag: NetworkTypeTag[TypedProtocol.CommonStates.Busy.type] =
    NetworkTypeTag.create("TypedProtocol.CommonStates.Busy")

  implicit val commonStatesDoneNetworkTypeTag: NetworkTypeTag[TypedProtocol.CommonStates.Done.type] =
    NetworkTypeTag.create("TypedProtocol.CommonStates.Done")

  implicit val commonMessagesStartNetworkTypeTag: NetworkTypeTag[TypedProtocol.CommonMessages.Start.type] =
    NetworkTypeTag.create("TypedProtocol.CommonMessages.Start")

  implicit val commonMessagesGetBlockIdNetworkTypeTag: NetworkTypeTag[TypedProtocol.CommonMessages.Get[BlockId]] =
    NetworkTypeTag.create("TypedProtocol.CommonMessages.Get[BlockId]")

  implicit val commonMessagesGetTransactionIdNetworkTypeTag
    : NetworkTypeTag[TypedProtocol.CommonMessages.Get[TransactionId]] =
    NetworkTypeTag.create("TypedProtocol.CommonMessages.Get[TransactionId]")

  implicit val commonMessagesGetLongTypedIdentifierOptNetworkTypeTag
    : NetworkTypeTag[TypedProtocol.CommonMessages.Get[(Long, Option[BlockId])]] =
    NetworkTypeTag.create("TypedProtocol.CommonMessages.Get[(Long, BlockId)]")

  implicit val commonMessagesGetLongTypedCurrentTipTypeTag: NetworkTypeTag[TypedProtocol.CommonMessages.Get[Long]] =
    NetworkTypeTag.create("TypedProtocol.CommonMessages.Get[Long]")

  implicit val commonMessagesResponseSlotDataNetworkTypeTag
    : NetworkTypeTag[TypedProtocol.CommonMessages.Response[SlotData]] =
    NetworkTypeTag.create("TypedProtocol.CommonMessages.Response[SlotData]")

  // TODO Remove after replace models
  implicit val commonMessagesResponseBlockHeaderNetworkTypeTag
    : NetworkTypeTag[TypedProtocol.CommonMessages.Response[BlockHeader]] =
    NetworkTypeTag.create("TypedProtocol.CommonMessages.Response[BlockHeader]")

  implicit val commonMessagesResponseBlockBodyNetworkTypeTag
    : NetworkTypeTag[TypedProtocol.CommonMessages.Response[BlockBody]] =
    NetworkTypeTag.create("TypedProtocol.CommonMessages.Response[BlockBody]")

  implicit val commonMessagesResponseTransactionNetworkTypeTag
    : NetworkTypeTag[TypedProtocol.CommonMessages.Response[IoTransaction]] =
    NetworkTypeTag.create("TypedProtocol.CommonMessages.Response[Transaction]")

  implicit val commonMessagesResponseTypedIdentifierNetworkTypeTag
    : NetworkTypeTag[TypedProtocol.CommonMessages.Response[BlockId]] =
    NetworkTypeTag.create("TypedProtocol.CommonMessages.Response[BlockId]")

  implicit val commonMessagesPushTypedIdentifierNetworkTypeTag
    : NetworkTypeTag[TypedProtocol.CommonMessages.Push[BlockId]] =
    NetworkTypeTag.create("TypedProtocol.CommonMessages.Push[BlockId]")

  implicit val commonMessagesPushTransactionIdNetworkTypeTag
    : NetworkTypeTag[TypedProtocol.CommonMessages.Push[TransactionId]] =
    NetworkTypeTag.create("TypedProtocol.CommonMessages.Push[TransactionId]")

  implicit val commonMessagesDoneNetworkTypeTag: NetworkTypeTag[TypedProtocol.CommonMessages.Done.type] =
    NetworkTypeTag.create("TypedProtocol.CommonMessages.Done")

  implicit val commonMessagesKnownHostsReq: NetworkTypeTag[TypedProtocol.CommonMessages.Get[CurrentKnownHostsReq]] =
    NetworkTypeTag.create("TypedProtocol.CommonMessages.CurrentKnownHostsReq")

  implicit val commonMessagesKnownHostsRes
    : NetworkTypeTag[TypedProtocol.CommonMessages.Response[CurrentKnownHostsRes]] =
    NetworkTypeTag.create("TypedProtocol.CommonMessages.CurrentKnownHostsRes")

  implicit val commonMessagesPingReq: NetworkTypeTag[TypedProtocol.CommonMessages.Get[PingMessage]] =
    NetworkTypeTag.create("TypedProtocol.CommonMessages.PingReq")

  implicit val commonMessagesPingRes: NetworkTypeTag[TypedProtocol.CommonMessages.Response[PongMessage]] =
    NetworkTypeTag.create("TypedProtocol.CommonMessages.PingRes")

  implicit val commonMessagesPeerServerPortReq: NetworkTypeTag[TypedProtocol.CommonMessages.Get[Unit]] =
    NetworkTypeTag.create("TypedProtocol.CommonMessages.PeerServerPortReq")

  implicit val commonMessagesPeerServerRes: NetworkTypeTag[TypedProtocol.CommonMessages.Response[KnownHost]] =
    NetworkTypeTag.create("TypedProtocol.CommonMessages.PeerServerRes")

  implicit val applicationLevelNotifyReq: NetworkTypeTag[TypedProtocol.CommonMessages.Get[Boolean]] =
    NetworkTypeTag.create("TypedProtocol.CommonMessages.applicationLevelNotifyReq")

  implicit val applicationLevelNotifyRes: NetworkTypeTag[TypedProtocol.CommonMessages.Response[Unit]] =
    NetworkTypeTag.create("TypedProtocol.CommonMessages.applicationLevelNotifyRes")
}
