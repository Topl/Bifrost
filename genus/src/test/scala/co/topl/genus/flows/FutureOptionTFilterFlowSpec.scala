package co.topl.genus.flows

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import akka.stream.scaladsl.{Sink, Source}
import cats.data.OptionT
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import co.topl.genus.typeclasses.implicits._
import org.scalatest.flatspec.AsyncFlatSpecLike
import org.scalatest.matchers.should.Matchers

class FutureOptionTFilterFlowSpec extends ScalaTestWithActorTestKit with AsyncFlatSpecLike with Matchers {

  behavior of "Future OptionT Filter Flow"

  it should "emit values returning some and ignore values returning none" in {
    val values = List(500, 4, 33, 199, 200)

    val underTest = FutureOptionTFilterFlow.create[IO, Int, Int](x => if (x % 2 == 0) OptionT.some(x) else OptionT.none)

    val result = Source(values).via(underTest).runWith(Sink.seq)

    result.map(values => values shouldBe List(500, 4, 200))
  }
}
