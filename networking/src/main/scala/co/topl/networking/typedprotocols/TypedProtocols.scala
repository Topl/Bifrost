package co.topl.networking.typedprotocols

import cats.Applicative
import cats.effect.kernel.Sync
import cats.implicits._
import co.topl.networking._

object PingPong {

  def make[F[_]: Sync]: F[TypedProtocolInitializer[F]] =
    Sync[F].delay {
      new TypedProtocolInitializer[F] {
        def initialPartyASubProtocol: F[TypedSubProtocol[F, _]] =
          SubProtocols.AwaitingPing[F]().pure[F].widen[TypedSubProtocol[F, _]]

        def initialPartyBSubProtocol: F[TypedSubProtocol[F, _]] =
          SubProtocols.Idle[F]().pure[F].widen[TypedSubProtocol[F, _]]
      }
    }

  object Messages {
    case object Go
    case object Ping
    case object Pong
  }

  object SubProtocols {

    case class Idle[F[_]: Applicative]() extends TypedSubProtocol[F, Messages.Go.type] {

      override def apply(
        message: ReceivableMessage
      ): F[(TypedSubProtocol[F, _], Option[TypedProtocol.Response], AgencyChange)] =
        (
          AwaitingPong[F]().pure[F].widen[TypedSubProtocol[F, _]],
          TypedProtocol.Message(Messages.Ping).some.widen[TypedProtocol.Response].pure[F],
          LocalIsAgent.pure[F].widen[AgencyChange]
        ).tupled
    }

    case class AwaitingPing[F[_]: Applicative]() extends TypedSubProtocol[F, Messages.Ping.type] {

      override def apply(
        message: ReceivableMessage
      ): F[(TypedSubProtocol[F, _], Option[TypedProtocol.Response], AgencyChange)] =
        (
          Idle[F]().pure[F].widen[TypedSubProtocol[F, _]],
          TypedProtocol.Message(Messages.Pong).some.widen[TypedProtocol.Response].pure[F],
          LocalIsAgent.pure[F].widen[AgencyChange]
        ).tupled
    }

    case class AwaitingPong[F[_]: Applicative]() extends TypedSubProtocol[F, Messages.Pong.type] {

      override def apply(
        message: ReceivableMessage
      ): F[(TypedSubProtocol[F, _], Option[TypedProtocol.Response], AgencyChange)] =
        (
          AwaitingPing[F]().pure[F].widen[TypedSubProtocol[F, _]],
          TypedProtocol.Message(Messages.Go).some.widen[TypedProtocol.Response].pure[F],
          LocalIsAgent.pure[F].widen[AgencyChange]
        ).tupled
    }
  }
}
