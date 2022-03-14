package co.topl.networking.typedprotocols

import cats.Applicative
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.implicits._
import co.topl.models._
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
    val handlerF = mockFunction[TypedIdentifier, F[Unit]]
    val transitions = new NotificationProtocols.Transaction.StateTransitionsClient[F](handlerF)
    val instance =
      TypedProtocolInstance(Parties.A)
        .withTransition(transitions.startNoneBusy)
        .withTransition(transitions.pushBusyBusy)
        .withTransition(transitions.doneBusyDone)

    val applier = instance.applier(TypedProtocolState(Parties.B.some, TypedProtocol.CommonStates.None)).unsafeRunSync()

    val computation =
      for {
        _ <- applier(TypedProtocol.CommonMessages.Start, Parties.B).map(_.value)
        _ = handlerF.expects(*).once().returning(Applicative[F].unit)
        _ <- applier(TypedProtocol.CommonMessages.Push(typedIdentifier), Parties.A).map(_.value)
        _ = handlerF.expects(*).once().returning(Applicative[F].unit)
        _ <- applier(TypedProtocol.CommonMessages.Push(typedIdentifier), Parties.A).map(_.value)
        _ = handlerF.expects(*).once().returning(Applicative[F].unit)
        _          <- applier(TypedProtocol.CommonMessages.Push(typedIdentifier), Parties.A).map(_.value)
        finalState <- applier(TypedProtocol.CommonMessages.Done, Parties.A).map(_.value)
      } yield finalState

    val finalState =
      computation.unsafeRunSync()

    finalState.currentState shouldBe TypedProtocol.CommonStates.Done

  }

  def typedIdentifier = TypedBytes(1: Byte, Bytes.fill(32)(0: Byte))
}
