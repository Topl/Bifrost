package co.topl.networking.typedprotocols.example

import cats.effect.IO
import cats.implicits._
import co.topl.networking.Parties
import co.topl.networking.typedprotocols.TypedProtocol
import co.topl.networking.typedprotocols.example.PingPong.StateTransitions._
import munit._

class PingPongSpec extends CatsEffectSuite {

  type F[A] = IO[A]

  import co.topl.networking.typedprotocols.example.PingPong._

  test("play ping pong") {
    val executor = TypedProtocol[F](Parties.A)

    for {
      none  <- ProtocolStates.None.pure[F]
      idle  <- executor(ProtocolMessages.Start)(none).nextState
      busy  <- executor(ProtocolMessages.Ping)(idle).nextState
      idle1 <- executor(ProtocolMessages.Pong)(busy).nextState
      done  <- executor(ProtocolMessages.Done)(idle1).nextState
    } yield done
  }
}
