package co.topl.networking.multiplexer

import akka.actor.ActorSystem
import akka.stream.scaladsl.{Flow, Keep}
import akka.stream.testkit.scaladsl.{TestSink, TestSource}
import akka.testkit.TestKitBase
import akka.util.ByteString
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterAll, EitherValues, OptionValues}
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

class MultiplexerSpec
    extends AnyFlatSpec
    with BeforeAndAfterAll
    with MockFactory
    with Matchers
    with ScalaCheckPropertyChecks
    with ScalaCheckDrivenPropertyChecks
    with EitherValues
    with OptionValues
    with TestKitBase {
  behavior of "Multiplexer"

  implicit lazy val system: ActorSystem = ActorSystem()

  it should "accept dynamic subProtocol sets" in {
    val subProtocolSet1 =
      List(
        SubHandler(1, Flow[ByteString].map(t => ByteString("echo") ++ t))
      )

    val (subHandlersProbe, subHandlersSource) = TestSource.probe[List[SubHandler]].preMaterialize()

    val underTest =
      MessageSerializerFramer()
        .via(Multiplexer(subHandlersSource))
        .via(MessageParserFramer())

    val (pub, sub) =
      TestSource.probe[(Byte, ByteString)].via(underTest).toMat(TestSink[(Byte, ByteString)]())(Keep.both).run()

    subHandlersProbe.sendNext(subProtocolSet1)

    forAll(Gen.asciiStr) { s: String =>
      sub.request(1)
      pub.sendNext((1: Byte) -> ByteString(s))
      sub.expectNext((1: Byte, ByteString("echo") ++ ByteString(s)))
    }

    val subProtocolSet2 =
      subProtocolSet1 ++
      List(
        SubHandler(2, Flow[ByteString].map(t => ByteString("echo2") ++ t))
      )
    subHandlersProbe.sendNext(subProtocolSet2)

    forAll(Gen.asciiStr, Gen.asciiStr) { (s1: String, s2: String) =>
      sub.request(1)
      pub.sendNext((1: Byte) -> ByteString(s1))
      sub.expectNext((1: Byte, ByteString("echo") ++ ByteString(s1)))
      sub.request(1)
      pub.sendNext((2: Byte) -> ByteString(s2))
      sub.expectNext((2: Byte, ByteString("echo2") ++ ByteString(s2)))
    }
  }

  it should "discard messages not belonging to an active subProtocol" in {

    val (subHandlersProbe, subHandlersSource) = TestSource.probe[List[SubHandler]].preMaterialize()

    val underTest =
      MessageSerializerFramer()
        .via(Multiplexer(subHandlersSource))
        .via(MessageParserFramer())

    val (pub, sub) =
      TestSource.probe[(Byte, ByteString)].via(underTest).toMat(TestSink[(Byte, ByteString)]())(Keep.both).run()

    subHandlersProbe.sendNext(Nil)

    forAll(Gen.asciiStr) { s: String =>
      sub.request(1)
      pub.sendNext((1: Byte) -> ByteString(s))
      sub.expectNoMessage()
    }
  }

  it should "discard messages belonging to a discarded subProtocol" in {
    val subProtocolSet1 =
      List(
        SubHandler(1, Flow[ByteString].map(t => ByteString("echo") ++ t))
      )

    val (subHandlersProbe, subHandlersSource) = TestSource.probe[List[SubHandler]].preMaterialize()

    val underTest =
      MessageSerializerFramer()
        .via(Multiplexer(subHandlersSource))
        .via(MessageParserFramer())

    val (pub, sub) =
      TestSource.probe[(Byte, ByteString)].via(underTest).toMat(TestSink[(Byte, ByteString)]())(Keep.both).run()

    subHandlersProbe.sendNext(subProtocolSet1)

    forAll(Gen.asciiStr) { s: String =>
      sub.request(1)
      pub.sendNext((1: Byte) -> ByteString(s))
      sub.expectNext((1: Byte, ByteString("echo") ++ ByteString(s)))
    }

    subHandlersProbe.sendNext(Nil)

    forAll(Gen.asciiStr) { s: String =>
      sub.request(1)
      pub.sendNext((1: Byte) -> ByteString(s))
      sub.expectNoMessage()
    }
  }
}
