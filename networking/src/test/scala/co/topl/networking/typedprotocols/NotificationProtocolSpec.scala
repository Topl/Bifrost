package co.topl.networking.typedprotocols

import cats.Applicative
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import co.topl.networking.Parties
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

    val applier = instance.applier(TypedProtocol.CommonStates.None).unsafeRunSync()

    val computation =
      for {
        _ <- applier(TypedProtocol.CommonMessages.Start, Parties.B).map(_.value)
        _ = handlerF.expects(*).once().returning(Applicative[F].unit)
        _ <- applier(TypedProtocol.CommonMessages.Push("foo"), Parties.A).map(_.value)
        _ = handlerF.expects(*).once().returning(Applicative[F].unit)
        _ <- applier(TypedProtocol.CommonMessages.Push("foo"), Parties.A).map(_.value)
        _ = handlerF.expects(*).once().returning(Applicative[F].unit)
        _          <- applier(TypedProtocol.CommonMessages.Push("foo"), Parties.A).map(_.value)
        finalState <- applier(TypedProtocol.CommonMessages.Done, Parties.A).map(_.value)
      } yield finalState

    val finalState =
      computation.unsafeRunSync()

    finalState shouldBe TypedProtocol.CommonStates.Done

  }

}
