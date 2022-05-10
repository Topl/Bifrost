package co.topl.genus.flows

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import akka.stream.scaladsl.{Keep, Sink, Source}
import org.scalatest.flatspec.AsyncFlatSpecLike
import org.scalatest.matchers.should.Matchers

class TrackLatestFlowSpec extends ScalaTestWithActorTestKit with AsyncFlatSpecLike with Matchers {
  behavior of "Track Latest Flow"

  it should "materialize to a function which returns the latest value seen" in {
    val upstream = Source(List(1, 2, 3, 4))

    val expectedLatest = 4

    val underTest = TrackLatestFlow.create(0)

    val resultFuture =
      upstream
        .viaMat(underTest)(Keep.right)
        .toMat(Sink.ignore)((getLatest, onComplete) =>
          onComplete.map(_ => getLatest())(testKit.system.executionContext)
        )
        .run()

    resultFuture.map { result =>
      result shouldBe expectedLatest
    }
  }

  it should "materialize to a function which returns the default value when no values seen" in {
    val upstream = Source.empty[Int]

    val default = 100

    val underTest = TrackLatestFlow.create(default)

    val resultFuture =
      upstream
        .viaMat(underTest)(Keep.right)
        // return the latest value when upstream completes
        .toMat(Sink.ignore)((getLatest, onComplete) =>
          onComplete.map(_ => getLatest())(testKit.system.executionContext)
        )
        .run()

    resultFuture.map { result =>
      result shouldBe default
    }
  }
}
