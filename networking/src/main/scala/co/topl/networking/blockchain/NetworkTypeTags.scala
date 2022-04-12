package co.topl.networking.blockchain

import co.topl.models.{BlockBodyV2, BlockHeaderV2, Transaction, TypedIdentifier}
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

  implicit val commonMessagesResponseBlockHeaderNetworkTypeTag
    : NetworkTypeTag[TypedProtocol.CommonMessages.Response[BlockHeaderV2]] =
    NetworkTypeTag.create("TypedProtocol.CommonMessages.Response[BlockHeaderV2]")

  implicit val commonMessagesResponseBlockBodyNetworkTypeTag
    : NetworkTypeTag[TypedProtocol.CommonMessages.Response[BlockBodyV2]] =
    NetworkTypeTag.create("TypedProtocol.CommonMessages.Response[BlockBodyV2]")

  implicit val commonMessagesResponseTransactionNetworkTypeTag
    : NetworkTypeTag[TypedProtocol.CommonMessages.Response[Transaction]] =
    NetworkTypeTag.create("TypedProtocol.CommonMessages.Response[Transaction]")

  implicit val commonMessagesPushTypedIdentifierNetworkTypeTag
    : NetworkTypeTag[TypedProtocol.CommonMessages.Push[TypedIdentifier]] =
    NetworkTypeTag.create("TypedProtocol.CommonMessages.Push[TypedIdentifier]")

  implicit val commonMessagesDoneNetworkTypeTag: NetworkTypeTag[TypedProtocol.CommonMessages.Done.type] =
    NetworkTypeTag.create("TypedProtocol.CommonMessages.Done")
}
