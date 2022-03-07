package co.topl.networking.typedprotocols

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.implicits._
import co.topl.networking.typedprotocols.PingPong.StateTransitions._
import co.topl.networking.{Parties, TypedProtocol, TypedProtocolState}
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterAll, EitherValues, OptionValues}
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

class PingPongSpec
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
  import PingPong._

  it should "play ping pong" in {

    val executor = TypedProtocol[F](Parties.A)

    val computation =
      for {
        none                                   <- TypedProtocolState(Parties.B.some, States.None()).pure[F]
        idle: TypedProtocolState[States.Idle]  <- executor(Messages.Start())(none).nextState
        busy: TypedProtocolState[States.Busy]  <- executor(Messages.Ping())(idle).nextState
        idle1: TypedProtocolState[States.Idle] <- executor(Messages.Pong())(busy).nextState
        done: TypedProtocolState[States.Done]  <- executor(Messages.Done())(idle1).nextState
      } yield done

    val protocol5 = computation.unsafeRunSync()

    protocol5.currentAgent shouldBe None
    protocol5.currentState shouldBe a[States.Done]

  }
}
