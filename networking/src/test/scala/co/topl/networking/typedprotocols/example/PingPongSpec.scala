package co.topl.networking.typedprotocols.example

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.implicits._
import co.topl.networking.Parties
import co.topl.networking.typedprotocols.TypedProtocol
import co.topl.networking.typedprotocols.example.PingPong.StateTransitions._
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
  import co.topl.networking.typedprotocols.example.PingPong._

  it should "play ping pong" in {

    val executor = TypedProtocol[F](Parties.A)

    val computation =
      for {
        none  <- ProtocolStates.None.pure[F]
        idle  <- executor(ProtocolMessages.Start)(none).nextState
        busy  <- executor(ProtocolMessages.Ping)(idle).nextState
        idle1 <- executor(ProtocolMessages.Pong)(busy).nextState
        done  <- executor(ProtocolMessages.Done)(idle1).nextState
      } yield done

    val protocol5 = computation.unsafeRunSync()

    protocol5 shouldBe ProtocolStates.Done

  }
}
