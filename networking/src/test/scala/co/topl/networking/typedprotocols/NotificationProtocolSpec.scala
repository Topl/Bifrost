package co.topl.networking.typedprotocols

import cats.effect.{Deferred, IO}
import cats.effect.unsafe.implicits.global
import co.topl.networking.{NetworkTypeTag, Parties}
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterAll, EitherValues, OptionValues}
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

class NotificationProtocolSpec
    extends AnyFlatSpec
    with BeforeAndAfterAll
    with MockFactory
    with Matchers
    with ScalaCheckPropertyChecks
    with ScalaCheckDrivenPropertyChecks
    with EitherValues
    with OptionValues {

  type F[A] = IO[A]

  behavior of "NotificationProtocol"
  import NotificationProtocolSpec._

  it should "run messages" in {
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
      .applier(TypedProtocol.CommonStates.None)
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
      .unsafeRunSync()
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
