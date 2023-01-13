package co.topl.networking.blockchain

import co.topl.models.{BlockBody, BlockHeader, SlotData, Transaction, TypedIdentifier}
import co.topl.networking.NetworkTypeTag
import co.topl.networking.typedprotocols.TypedProtocol

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

  implicit val commonMessagesGetTypedIdentifierNetworkTypeTag
    : NetworkTypeTag[TypedProtocol.CommonMessages.Get[TypedIdentifier]] =
    NetworkTypeTag.create("TypedProtocol.CommonMessages.Get[TypedIdentifier]")

  implicit val commonMessagesGetLongTypedIdentifierOptNetworkTypeTag
    : NetworkTypeTag[TypedProtocol.CommonMessages.Get[(Long, Option[TypedIdentifier])]] =
    NetworkTypeTag.create("TypedProtocol.CommonMessages.Get[(Long, TypedIdentifier)]")

  implicit val commonMessagesResponseSlotDataNetworkTypeTag
    : NetworkTypeTag[TypedProtocol.CommonMessages.Response[SlotData]] =
    NetworkTypeTag.create("TypedProtocol.CommonMessages.Response[SlotData]")

  // TODO Remove after replace models
  implicit val commonMessagesResponseBlockHeaderNetworkTypeTag
    : NetworkTypeTag[TypedProtocol.CommonMessages.Response[BlockHeader]] =
    NetworkTypeTag.create("TypedProtocol.CommonMessages.Response[BlockHeader]")

  implicit val commonMessagesResponseConsunsesBlockHeaderNetworkTypeTag
    : NetworkTypeTag[TypedProtocol.CommonMessages.Response[co.topl.consensus.models.BlockHeader]] =
    NetworkTypeTag.create("TypedProtocol.CommonMessages.Response[BlockHeader]")

  // TODO Remove after replace models
  implicit val commonMessagesResponseBlockBodyNetworkTypeTag
    : NetworkTypeTag[TypedProtocol.CommonMessages.Response[BlockBody]] =
    NetworkTypeTag.create("TypedProtocol.CommonMessages.Response[BlockBody]")

  implicit val commonMessagesResponseNodeBlockBodyNetworkTypeTag
    : NetworkTypeTag[TypedProtocol.CommonMessages.Response[co.topl.node.models.BlockBody]] =
    NetworkTypeTag.create("TypedProtocol.CommonMessages.Response[BlockBody]")

  implicit val commonMessagesResponseTransactionNetworkTypeTag
    : NetworkTypeTag[TypedProtocol.CommonMessages.Response[Transaction]] =
    NetworkTypeTag.create("TypedProtocol.CommonMessages.Response[Transaction]")

  implicit val commonMessagesResponseTypedIdentifierNetworkTypeTag
    : NetworkTypeTag[TypedProtocol.CommonMessages.Response[TypedIdentifier]] =
    NetworkTypeTag.create("TypedProtocol.CommonMessages.Response[TypedIdentifier]")

  implicit val commonMessagesPushTypedIdentifierNetworkTypeTag
    : NetworkTypeTag[TypedProtocol.CommonMessages.Push[TypedIdentifier]] =
    NetworkTypeTag.create("TypedProtocol.CommonMessages.Push[TypedIdentifier]")

  implicit val commonMessagesDoneNetworkTypeTag: NetworkTypeTag[TypedProtocol.CommonMessages.Done.type] =
    NetworkTypeTag.create("TypedProtocol.CommonMessages.Done")
}
