package co.topl.networking.typedprotocols

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import co.topl.networking.{Parties, TypedProtocol}
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

  behavior of "PingPong"

  it should "start awaiting ping for party A" in {
    val local = Parties.A
    val protocol = (for {
      initializer <- PingPong.make[F]
      protocol    <- TypedProtocol.Eval.make[F](local, initializer)
    } yield protocol).unsafeRunSync()

    protocol.currentAgentParty.unsafeRunSync() shouldBe Parties.A
    protocol.currentSubProtocol.unsafeRunSync() shouldBe a[PingPong.SubProtocols.AwaitingPing[F]]

    val responseBad = protocol.receive(PingPong.Messages.Go).unsafeRunSync().value

    responseBad shouldBe a[TypedProtocol.Fail]

    val response = protocol.receive(PingPong.Messages.Ping).unsafeRunSync().value

    response shouldBe a[TypedProtocol.Message[PingPong.Messages.Pong.type]]
  }

  it should "start idle for party B" in {
    val local = Parties.B
    val protocol = (for {
      initializer <- PingPong.make[F]
      protocol    <- TypedProtocol.Eval.make[F](local, initializer)
    } yield protocol).unsafeRunSync()

    protocol.currentAgentParty.unsafeRunSync() shouldBe Parties.A
    protocol.currentSubProtocol.unsafeRunSync() shouldBe a[PingPong.SubProtocols.Idle[F]]

    val responseBad = protocol.receive(PingPong.Messages.Pong).unsafeRunSync().value

    responseBad shouldBe a[TypedProtocol.Fail]

    val response = protocol.receive(PingPong.Messages.Go).unsafeRunSync().value

    response shouldBe a[TypedProtocol.Message[PingPong.Messages.Ping.type]]
  }

  it should "play ping pong" in {
    val (protocolA, protocolB) = (for {
      initializer <- PingPong.make[F]
      protocolA   <- TypedProtocol.Eval.make[F](Parties.A, initializer)
      protocolB   <- TypedProtocol.Eval.make[F](Parties.B, initializer)
    } yield (protocolA, protocolB)).unsafeRunSync()

    var toSend: Any = PingPong.Messages.Go

    for (_ <- 0 to 4) {
      println(s"Party A sends $toSend to Party B")
      val TypedProtocol.Message(bResponse) = protocolB.receive(toSend).unsafeRunSync().value
      println(s"Party B sends $bResponse to Party A")
      val TypedProtocol.Message(aResponse) = protocolA.receive(bResponse).unsafeRunSync().value
      toSend = aResponse
    }
  }
}
