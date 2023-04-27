package co.topl.networking.blockchain

import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.consensus.models.BlockHeader
import co.topl.consensus.models.BlockId
import co.topl.consensus.models.SlotData
import co.topl.networking.NetworkTypeTag
import co.topl.networking.typedprotocols.TypedProtocol
import co.topl.node.models.BlockBody

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

  implicit val commonMessagesGetUnitTypedCurrentTipTypeTag: NetworkTypeTag[TypedProtocol.CommonMessages.Get[Unit]] =
    NetworkTypeTag.create("TypedProtocol.CommonMessages.Get[Unit]")

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
}
