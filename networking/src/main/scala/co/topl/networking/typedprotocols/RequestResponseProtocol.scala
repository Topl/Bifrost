package co.topl.networking.typedprotocols

import cats.Applicative
import cats.implicits._
import co.topl.models.TypedIdentifier
import co.topl.networking.Parties

trait RequestResponseProtocol[T, Query] {

  class ServerStateTransitions[F[_]: Applicative](fetch: Query => F[Option[T]]) {

    implicit val startNoneIdle: StateTransition[
      F,
      TypedProtocol.CommonMessages.Start.type,
      TypedProtocol.CommonStates.None.type,
      TypedProtocol.CommonStates.Idle.type
    ] =
      (_, _, _) => TypedProtocolState(Parties.B.some, TypedProtocol.CommonStates.Idle).pure[F]

    implicit val getIdleBusy: StateTransition[
      F,
      TypedProtocol.CommonMessages.Get[Query],
      TypedProtocol.CommonStates.Idle.type,
      TypedProtocol.CommonStates.Busy.type
    ] =
      (message, _, _) => fetch(message.query).as(TypedProtocolState(Parties.A.some, TypedProtocol.CommonStates.Busy))

    implicit val responseBusyIdle: StateTransition[F, TypedProtocol.CommonMessages.Response[
      T
    ], TypedProtocol.CommonStates.Busy.type, TypedProtocol.CommonStates.Idle.type] =
      (_, _, _) => TypedProtocolState(Parties.B.some, TypedProtocol.CommonStates.Idle).pure[F]

    implicit val doneIdleDone: StateTransition[
      F,
      TypedProtocol.CommonMessages.Done.type,
      TypedProtocol.CommonStates.Idle.type,
      TypedProtocol.CommonStates.Done.type
    ] =
      (_, _, _) => TypedProtocolState(none, TypedProtocol.CommonStates.Done).pure[F]
  }

  class ClientStateTransitions[F[_]: Applicative](responseReceived: Option[T] => F[Unit]) {

    implicit val startNoneIdle: StateTransition[
      F,
      TypedProtocol.CommonMessages.Start.type,
      TypedProtocol.CommonStates.None.type,
      TypedProtocol.CommonStates.Idle.type
    ] =
      (_, _, _) => TypedProtocolState(Parties.B.some, TypedProtocol.CommonStates.Idle).pure[F]

    implicit val getIdleBusy: StateTransition[
      F,
      TypedProtocol.CommonMessages.Get[Query],
      TypedProtocol.CommonStates.Idle.type,
      TypedProtocol.CommonStates.Busy.type
    ] =
      (_, _, _) => TypedProtocolState(Parties.A.some, TypedProtocol.CommonStates.Busy).pure[F]

    implicit val responseBusyIdle: StateTransition[F, TypedProtocol.CommonMessages.Response[
      T
    ], TypedProtocol.CommonStates.Busy.type, TypedProtocol.CommonStates.Idle.type] =
      (message, _, _) =>
        responseReceived(message.dataOpt).as(TypedProtocolState(Parties.B.some, TypedProtocol.CommonStates.Idle))

    implicit val doneIdleDone: StateTransition[
      F,
      TypedProtocol.CommonMessages.Done.type,
      TypedProtocol.CommonStates.Idle.type,
      TypedProtocol.CommonStates.Done.type
    ] =
      (_, _, _) => TypedProtocolState(none, TypedProtocol.CommonStates.Done).pure[F]
  }

}

object RequestResponseProtocols {
  object PingPong extends RequestResponseProtocol[Unit, Unit]
  object KeepAlive extends RequestResponseProtocol[Long, Long] // Long = Cookie = Random number
  object SlotData extends RequestResponseProtocol[co.topl.models.SlotData, TypedIdentifier]
  object Header extends RequestResponseProtocol[co.topl.models.BlockHeaderV2, TypedIdentifier]
  object Body extends RequestResponseProtocol[co.topl.models.BlockBodyV2, TypedIdentifier]
  object Transaction extends RequestResponseProtocol[co.topl.models.Transaction, TypedIdentifier]

  // DownloadProtocol#Query is of the form (height, optionalLocalId)
  object BlockIdAtHeight
      extends RequestResponseProtocol[co.topl.models.TypedIdentifier, (Long, Option[TypedIdentifier])]
}
