package co.topl.networking.typedprotocols

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.implicits._
import co.topl.networking.typedprotocols.PingPong1.StateTransitions._
import co.topl.networking.{Parties, TypedProtocol1, TypedProtocolState}
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterAll, EitherValues, OptionValues}
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

class PingPong1Spec
    extends AnyFlatSpec
    with BeforeAndAfterAll
    with MockFactory
    with Matchers
    with ScalaCheckPropertyChecks
    with ScalaCheckDrivenPropertyChecks
    with EitherValues
    with OptionValues {

  type F[A] = IO[A]

  behavior of "PingPong1"
  import PingPong1._

  it should "play ping pong" in {

    val executor = TypedProtocol1[F](Parties.A)

    val computation =
      for {
        none                                   <- TypedProtocolState(Parties.B.some, States.None()).pure[F]
        idle: TypedProtocolState[States.Idle]  <- executor(Messages.Start())(none.currentState).nextState
        busy: TypedProtocolState[States.Busy]  <- executor(Messages.Ping())(idle.currentState).nextState
        idle1: TypedProtocolState[States.Idle] <- executor(Messages.Pong())(busy.currentState).nextState
        done: TypedProtocolState[States.Done]  <- executor(Messages.Done())(idle1.currentState).nextState
      } yield done

    val protocol5 = computation.unsafeRunSync()

    protocol5.currentAgent shouldBe None
    protocol5.currentState shouldBe a[States.Done]

  }
}
