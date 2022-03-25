package co.topl.networking.typedprotocols

import cats.Applicative
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.implicits._
import co.topl.networking.Parties
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

    val applier = instance.applier(TypedProtocol.CommonStates.None).unsafeRunSync()

    val computation =
      for {
        _ <- applier(TypedProtocol.CommonMessages.Start, Parties.B).map(_.value)
        _ = handlerF.expects(*).once().returning(Applicative[F].unit)
        _ <- applier(TypedProtocol.CommonMessages.Get("foo"), Parties.B).map(_.value)
        _ = handlerF.expects(*).never()
        _ <- applier(TypedProtocol.CommonMessages.Response(none), Parties.A).map(_.value)
        _ = handlerF.expects(*).once().returning(Applicative[F].unit)
        _ <- applier(TypedProtocol.CommonMessages.Get("foo"), Parties.B).map(_.value)
        _ = handlerF.expects(*).never()
        _          <- applier(TypedProtocol.CommonMessages.Response(none), Parties.A).map(_.value)
        finalState <- applier(TypedProtocol.CommonMessages.Done, Parties.B).map(_.value)
      } yield finalState

    val finalState = computation.unsafeRunSync()

    finalState shouldBe TypedProtocol.CommonStates.Done

  }

}
