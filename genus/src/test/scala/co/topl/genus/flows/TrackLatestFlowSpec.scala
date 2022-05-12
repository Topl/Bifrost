package co.topl.genus.flows

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import akka.stream.scaladsl.{Keep, Sink}
import akka.stream.testkit.scaladsl.TestSource
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration.DurationInt
import scala.util.Try

class TrackLatestFlowSpec extends ScalaTestWithActorTestKit with AnyFlatSpecLike with Matchers with ScalaFutures {
  behavior of "Track Latest Flow"

  it should "materialize to a function which returns the latest value seen" in {
    val upstream = TestSource[Int]()

    val underTest = TrackLatestFlow.create[Int]

    val lastElement = 5

    val (probe, getLatest) = upstream.toMat(underTest.to(Sink.ignore))(Keep.both).run()

    probe
      .sendNext(1)
      .sendNext(3)
      .sendNext(lastElement)

    getLatest().futureValue shouldBe lastElement
  }

  it should "materialize to a function which returns a never-completed future if no values seen" in {
    val upstream = TestSource[Int]()

    val underTest = TrackLatestFlow.create[Int]

    val (probe, getLatest) = upstream.toMat(underTest.to(Sink.ignore))(Keep.both).run()

    probe.sendComplete()

    Try(getLatest().futureValue(Timeout(500.milliseconds))).isFailure shouldBe true
  }
}
