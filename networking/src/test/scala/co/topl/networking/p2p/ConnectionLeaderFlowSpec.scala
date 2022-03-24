package co.topl.networking.p2p

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.scaladsl.{Flow, Keep}
import akka.stream.testkit.scaladsl.{TestSink, TestSource}
import akka.testkit.TestKitBase
import akka.util.ByteString
import cats.effect.IO
import org.scalamock.scalatest.MockFactory
import org.scalatest.{BeforeAndAfterAll, EitherValues, OptionValues}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}
import co.topl.networking.multiplexer._

class ConnectionLeaderFlowSpec
    extends AnyFlatSpec
    with BeforeAndAfterAll
    with MockFactory
    with Matchers
    with ScalaCheckPropertyChecks
    with ScalaCheckDrivenPropertyChecks
    with EitherValues
    with OptionValues
    with TestKitBase {
  behavior of "ConnectionLeaderFlow"

  type F[A] = IO[A]

  implicit lazy val system: ActorSystem = ActorSystem()

  it should "establish a connection leader as local if the local number is greater than the remote's" in {
    val subFlowF = mockFunction[ConnectionLeader, Flow[ByteString, ByteString, NotUsed]]

    subFlowF.expects(ConnectionLeaders.Local).once().returning(Flow[ByteString])

    val underTest = ConnectionLeaderFlow(subFlowF)

    val (pub, sub) =
      TestSource.probe[ByteString].via(underTest).toMat(TestSink[ByteString]())(Keep.both).run()

    sub.request(1)
    val data = bytestringToInt(sub.expectNext())
    pub.sendNext(intToBytestring(data - 1))

    sub.request(1)
    pub.sendNext(ByteString("data"))
    sub.expectNext(ByteString("data"))
  }

}
