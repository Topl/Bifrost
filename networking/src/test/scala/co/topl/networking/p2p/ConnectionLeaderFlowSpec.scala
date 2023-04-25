package co.topl.networking.p2p

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.scaladsl.Flow
import akka.stream.scaladsl.Keep
import akka.stream.testkit.scaladsl.TestSink
import akka.stream.testkit.scaladsl.TestSource
import akka.testkit.TestKitBase
import akka.util.ByteString
import cats.effect.IO
import co.topl.crypto.hash.Blake2b256
import co.topl.networking.multiplexer._
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.BeforeAndAfterAll
import org.scalatest.EitherValues
import org.scalatest.OptionValues
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.util.Random

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

  it should "establish a connection leader as local if " +
  "BigInt(localNumberBytes ++ remoteNumberBytes) > BigInt(remoteNumberBytes ++ localNumberBytes)" in {

    implicit val random1 = new Random(0)
    val random2 = new Random(0)
    val expectedPublishedInt = random2.nextInt()
    val expectedPublishedEvidence =
      new Blake2b256().hash(intToBytestring(expectedPublishedInt).toArray)

    val remoteInt = expectedPublishedInt + 1
    val remoteIntEvidence = new Blake2b256().hash(intToBytestring(remoteInt).toArray)

    val expectedConnectionLeader =
      if (
        BigInt(
          new Blake2b256()
            .hash(intToBytestring(expectedPublishedInt).toArray ++ intToBytestring(remoteInt).toArray)
        ) >
        BigInt(
          new Blake2b256()
            .hash(intToBytestring(remoteInt).toArray ++ intToBytestring(expectedPublishedInt).toArray)
        )
      ) ConnectionLeaders.Local
      else ConnectionLeaders.Remote

    val subFlowF = mockFunction[ConnectionLeader, Flow[ByteString, ByteString, NotUsed]]

    subFlowF.expects(expectedConnectionLeader).once().returning(Flow[ByteString])

    val underTest = ConnectionLeaderFlow(subFlowF)

    val (pub, sub) =
      TestSource.probe[ByteString].via(underTest).toMat(TestSink[ByteString]())(Keep.both).run()

    sub.request(1)
    val evidence = sub.expectNext().toArray
    evidence shouldBe expectedPublishedEvidence
    pub.sendNext(ByteString(remoteIntEvidence))

    sub.request(1)
    bytestringToInt(sub.expectNext()) shouldBe expectedPublishedInt
    pub.sendNext(intToBytestring(remoteInt))

    sub.request(1)
    pub.sendNext(ByteString("data"))
    sub.expectNext(ByteString("data"))
  }

}
