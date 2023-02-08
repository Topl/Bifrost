package co.topl.networking.typedprotocols

import cats.Applicative
import cats.effect.{Deferred, IO}
import cats.effect.unsafe.implicits.global
import cats.implicits._
import co.topl.networking.{NetworkTypeTag, Parties}
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterAll, EitherValues, OptionValues}
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

class RequestResponseProtocolSpec
    extends AnyFlatSpec
    with BeforeAndAfterAll
    with MockFactory
    with Matchers
    with ScalaCheckPropertyChecks
    with ScalaCheckDrivenPropertyChecks
    with EitherValues
    with OptionValues {
  import RequestResponseProtocolSpec._

  type F[A] = IO[A]

  behavior of "RequestResponseProtocol"

  it should "run messages" in {
    val handlerF = mockFunction[String, F[Unit]]
    val instance = {
      val transitions = {
        val protocol = new RequestResponseProtocol[String, Int] {}
        new protocol.ServerStateTransitions[F](handlerF)
      }
      import transitions._
      TypedProtocolInstance(Parties.A)
        .withTransition(startNoneIdle)
        .withTransition(getIdleBusy)
        .withTransition(responseBusyIdle)
        .withTransition(doneIdleDone)
    }

    instance
      .applier(TypedProtocol.CommonStates.None)(_ => Applicative[F].unit)
      .use { applier =>
        for {
          _ <- applier(TypedProtocol.CommonMessages.Start, Parties.A)

          d1 <- Deferred[F, Unit]
          _ = handlerF.expects(*).once().returning(d1.complete(()).void)
          _ <- applier(TypedProtocol.CommonMessages.Get("foo"), Parties.B)
          _ <- d1.get

          _ = handlerF.expects(*).never()
          _ <- applier(TypedProtocol.CommonMessages.Response(none[Int]), Parties.A)

          d2 <- Deferred[F, Unit]
          _ = handlerF.expects(*).once().returning(d2.complete(()).void)
          _ <- applier(TypedProtocol.CommonMessages.Get("foo"), Parties.B)
          _ <- d2.get

          _ = handlerF.expects(*).never()
          _ <- applier(TypedProtocol.CommonMessages.Response(none[Int]), Parties.A)
          _ <- applier(TypedProtocol.CommonMessages.Done, Parties.B)
        } yield ()
      }
      .unsafeRunSync()
  }

}

object RequestResponseProtocolSpec {

  implicit val commonMessagesStartNetworkTypeTag: NetworkTypeTag[TypedProtocol.CommonMessages.Start.type] =
    NetworkTypeTag.create("TypedProtocol.CommonMessages.Start")

  implicit val commonStatesNoneNetworkTypeTag: NetworkTypeTag[TypedProtocol.CommonStates.None.type] =
    NetworkTypeTag.create("TypedProtocol.CommonStates.None")

  implicit val commonStatesIdleNetworkTypeTag: NetworkTypeTag[TypedProtocol.CommonStates.Idle.type] =
    NetworkTypeTag.create("TypedProtocol.CommonStates.Idle")

  implicit val commonStatesBusyNetworkTypeTag: NetworkTypeTag[TypedProtocol.CommonStates.Busy.type] =
    NetworkTypeTag.create("TypedProtocol.CommonStates.Busy")

  implicit val commonMessagesGetStringNetworkTypeTag: NetworkTypeTag[TypedProtocol.CommonMessages.Get[String]] =
    NetworkTypeTag.create("TypedProtocol.CommonMessages.Get[String]")

  implicit val commonMessagesResoibseIntNetworkTypeTag: NetworkTypeTag[TypedProtocol.CommonMessages.Response[Int]] =
    NetworkTypeTag.create("TypedProtocol.CommonMessages.Response[Int]")

  implicit val commonStatesDoneNetworkTypeTag: NetworkTypeTag[TypedProtocol.CommonStates.Done.type] =
    NetworkTypeTag.create("TypedProtocol.CommonStates.Done")

  implicit val commonMessagesDoneNetworkTypeTag: NetworkTypeTag[TypedProtocol.CommonMessages.Done.type] =
    NetworkTypeTag.create("TypedProtocol.CommonMessages.Done")
}
