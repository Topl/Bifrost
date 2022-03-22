package co.topl.networking.blockchain

import co.topl.models.{BlockBodyV2, BlockHeaderV2, Transaction, TypedIdentifier}
import co.topl.networking.NetworkTypeTag
import co.topl.networking.typedprotocols.TypedProtocol

object NetworkTypeTags {

  implicit val commonStatesNoneNetworkTypeTag: NetworkTypeTag[TypedProtocol.CommonStates.None.type] =
    NetworkTypeTag.create

  implicit val commonStatesIdleNetworkTypeTag: NetworkTypeTag[TypedProtocol.CommonStates.Idle.type] =
    NetworkTypeTag.create

  implicit val commonStatesBusyNetworkTypeTag: NetworkTypeTag[TypedProtocol.CommonStates.Busy.type] =
    NetworkTypeTag.create

  implicit val commonStatesDoneNetworkTypeTag: NetworkTypeTag[TypedProtocol.CommonStates.Done.type] =
    NetworkTypeTag.create

  implicit val commonMessagesStartNetworkTypeTag: NetworkTypeTag[TypedProtocol.CommonMessages.Start.type] =
    NetworkTypeTag.create

  implicit val commonMessagesGetTypedIdentifierNetworkTypeTag
    : NetworkTypeTag[TypedProtocol.CommonMessages.Get[TypedIdentifier]] =
    NetworkTypeTag.create

  implicit val commonMessagesResponseBlockHeaderNetworkTypeTag
    : NetworkTypeTag[TypedProtocol.CommonMessages.Response[BlockHeaderV2]] =
    NetworkTypeTag.create

  implicit val commonMessagesResponseBlockBodyNetworkTypeTag
    : NetworkTypeTag[TypedProtocol.CommonMessages.Response[BlockBodyV2]] =
    NetworkTypeTag.create

  implicit val commonMessagesResponseTransactionNetworkTypeTag
    : NetworkTypeTag[TypedProtocol.CommonMessages.Response[Transaction]] =
    NetworkTypeTag.create

  implicit val commonMessagesPushTypedIdentifierNetworkTypeTag
    : NetworkTypeTag[TypedProtocol.CommonMessages.Push[TypedIdentifier]] =
    NetworkTypeTag.create

  implicit val commonMessagesDoneNetworkTypeTag: NetworkTypeTag[TypedProtocol.CommonMessages.Done.type] =
    NetworkTypeTag.create
}
