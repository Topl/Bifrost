package co.topl.networking

import cats.effect.Ref
import cats.effect.kernel.Async
import cats.implicits._

import scala.reflect.ClassTag

trait TypedProtocol[F[_]] {
  def localParty: F[Party]
  def currentAgentParty: F[Party]
  def currentSubProtocol: F[TypedSubProtocol[F, _]]
  def receive[T: ClassTag](t: T): F[Option[TypedProtocol.Response]]
}

object TypedProtocol {

  sealed abstract class Response
  case class Message[T](data: T) extends Response
  case class Fail(reason: Throwable) extends Response
  case object Close extends Response

  object Eval {

    def make[F[_]: Async](local: Party, initializer: TypedProtocolInitializer[F]): F[TypedProtocol[F]] =
      for {
        subProtocol <- local.if_(ifA = initializer.initialPartyASubProtocol, ifB = initializer.initialPartyBSubProtocol)
        ref: Ref[F, (Party, TypedSubProtocol[F, Any])] <- Ref.of(
          (Parties.A: Party, subProtocol.asInstanceOf[TypedSubProtocol[F, Any]])
        )
      } yield new TypedProtocol[F] {
        def localParty: F[Party] = local.pure[F]

        def currentAgentParty: F[Party] = ref.get.map(_._1)

        def currentSubProtocol: F[TypedSubProtocol[F, _]] = ref.get.map(_._2)

        def receive[T: ClassTag](t: T): F[Option[TypedProtocol.Response]] =
          currentSubProtocol.flatMap(p =>
            if (p.rClassTag.runtimeClass == t.getClass) {
              for {
                (newSubProtocol, response, agencyChange) <- p.apply(t.asInstanceOf[p.ReceivableMessage])
                _ <- ref.update { case (p, _) =>
                  val newAgent =
                    agencyChange match {
                      case LocalIsAgent  => p
                      case RemoteIsAgent => p.opposite
                    }
                  (newAgent, newSubProtocol.asInstanceOf[TypedSubProtocol[F, Any]])
                }
              } yield response
            } else
              TypedProtocol
                .Fail(new IllegalArgumentException(s"Invalid argument exception $t"))
                .some
                .widen[TypedProtocol.Response]
                .pure[F]
          )
      }
  }
}

trait TypedProtocolInitializer[F[_]] {

  def initialPartyASubProtocol: F[TypedSubProtocol[F, _]]

  def initialPartyBSubProtocol: F[TypedSubProtocol[F, _]]

}

abstract class TypedSubProtocol[F[_], R](implicit val rClassTag: ClassTag[R]) {

  type ReceivableMessage = R

  def apply(message: ReceivableMessage): F[(TypedSubProtocol[F, _], Option[TypedProtocol.Response], AgencyChange)]

}

sealed abstract class AgencyChange
case object LocalIsAgent extends AgencyChange
case object RemoteIsAgent extends AgencyChange
