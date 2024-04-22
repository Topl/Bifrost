package co.topl.networking.legacy

import cats.Applicative
import cats.effect.{Deferred, IO}
import co.topl.algebras.testInterpreters.NoOpLogger
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalamock.munit.AsyncMockFactory
import org.typelevel.log4cats.Logger

class NotificationProtocolSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {

  type F[A] = IO[A]
  import NotificationProtocolSpec._

  implicit private val logger: Logger[F] = new NoOpLogger[F]

  test("run messages") {
    withMock {
      val handlerF = mockFunction[String, F[Unit]]
      val instance = {
        val transitions = {
          val protocol = new NotificationProtocol[String] {}
          new protocol.StateTransitionsClient[F](handlerF)
        }
        import transitions._

        TypedProtocolInstance(Parties.B)
          .withTransition(startNoneBusy)
          .withTransition(pushBusyBusy)
          .withTransition(doneBusyDone)
      }
      instance
        .applier(TypedProtocol.CommonStates.None)(_ => Applicative[F].unit)
        .use { applier =>
          for {
            _ <- applier(TypedProtocol.CommonMessages.Start, Parties.B)

            d1 <- Deferred[F, Unit]
            _ = handlerF.expects(*).once().returning(d1.complete(()).void)
            _ <- applier(TypedProtocol.CommonMessages.Push("foo"), Parties.A)
            _ <- d1.get

            d2 <- Deferred[F, Unit]
            _ = handlerF.expects(*).once().returning(d2.complete(()).void)
            _ <- applier(TypedProtocol.CommonMessages.Push("foo"), Parties.A)
            _ <- d2.get

            d3 <- Deferred[F, Unit]
            _ = handlerF.expects(*).once().returning(d3.complete(()).void)
            _ <- applier(TypedProtocol.CommonMessages.Push("foo"), Parties.A)
            _ <- d3.get

            _ <- applier(TypedProtocol.CommonMessages.Done, Parties.A)
          } yield ()
        }
    }
  }

}

private object NotificationProtocolSpec {

  implicit val commonMessagesStartNetworkTypeTag: NetworkTypeTag[TypedProtocol.CommonMessages.Start.type] =
    NetworkTypeTag.create("TypedProtocol.CommonMessages.Start")

  implicit val commonStatesNoneNetworkTypeTag: NetworkTypeTag[TypedProtocol.CommonStates.None.type] =
    NetworkTypeTag.create("TypedProtocol.CommonStates.None")

  implicit val commonStatesBusyNetworkTypeTag: NetworkTypeTag[TypedProtocol.CommonStates.Busy.type] =
    NetworkTypeTag.create("TypedProtocol.CommonStates.Busy")

  implicit val commonMessagesPushStringNetworkTypeTag: NetworkTypeTag[TypedProtocol.CommonMessages.Push[String]] =
    NetworkTypeTag.create("TypedProtocol.CommonMessages.Push[String]")

  implicit val commonStatesDoneNetworkTypeTag: NetworkTypeTag[TypedProtocol.CommonStates.Done.type] =
    NetworkTypeTag.create("TypedProtocol.CommonStates.Done")

  implicit val commonMessagesDoneNetworkTypeTag: NetworkTypeTag[TypedProtocol.CommonMessages.Done.type] =
    NetworkTypeTag.create("TypedProtocol.CommonMessages.Done")

}
