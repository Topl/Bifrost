package co.topl.networking.multiplexer

import akka.actor.ActorSystem
import akka.stream.scaladsl.{Keep, Sink, Source}
import akka.stream.testkit.scaladsl.{TestSink, TestSource}
import akka.testkit.TestKitBase
import akka.util.ByteString
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterAll, EitherValues, OptionValues}
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

class DynamicMultiplexerSpec
    extends AnyFlatSpec
    with BeforeAndAfterAll
    with MockFactory
    with Matchers
    with ScalaCheckPropertyChecks
    with ScalaCheckDrivenPropertyChecks
    with EitherValues
    with OptionValues
    with TestKitBase {
  behavior of "DynamicMultiplexer"

  implicit lazy val system: ActorSystem = ActorSystem()

  it should "accept dynamic subProtocol sets" in {
    val subProtocolSet1 =
      List(
        {
          val (source, sink) = echo1
          SubHandler(1, sink, source)
        }
      )

    val (subHandlersProbe, subHandlersSource) = TestSource.probe[List[SubHandler]].preMaterialize()

    val underTest =
      MessageSerializerFramer()
        .via(DynamicMultiplexer(subHandlersSource))
        .via(MessageParserFramer())

    val (pub, sub) =
      TestSource.probe[(Byte, ByteString)].via(underTest).toMat(TestSink[(Byte, ByteString)]())(Keep.both).run()

    subHandlersProbe.sendNext(subProtocolSet1)

    forAll(Gen.asciiStr) { s: String =>
      sub.request(1)
      pub.sendNext((1: Byte) -> ByteString(s))
      sub.expectNext((1: Byte, ByteString("echo1") ++ ByteString(s)))
    }

    val subProtocolSet2 =
      subProtocolSet1 ++
      List(
        {
          val (source, sink) = echo2
          SubHandler(2, sink, source)
        }
      )
    subHandlersProbe.sendNext(subProtocolSet2)

    forAll(Gen.asciiStr, Gen.asciiStr) { (s1: String, s2: String) =>
      sub.request(1)
      pub.sendNext((1: Byte) -> ByteString(s1))
      sub.expectNext((1: Byte, ByteString("echo1") ++ ByteString(s1)))
      sub.request(1)
      pub.sendNext((2: Byte) -> ByteString(s2))
      sub.expectNext((2: Byte, ByteString("echo2") ++ ByteString(s2)))
    }
  }

  it should "discard messages not belonging to an active subProtocol" in {

    val (subHandlersProbe, subHandlersSource) = TestSource.probe[List[SubHandler]].preMaterialize()

    val underTest =
      MessageSerializerFramer()
        .via(DynamicMultiplexer(subHandlersSource))
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
        {
          val (source, sink) = echo1
          SubHandler(1, sink, source)
        }
      )

    val (subHandlersProbe, subHandlersSource) = TestSource.probe[List[SubHandler]].preMaterialize()

    val underTest =
      MessageSerializerFramer()
        .via(DynamicMultiplexer(subHandlersSource))
        .via(MessageParserFramer())

    val (pub, sub) =
      TestSource.probe[(Byte, ByteString)].via(underTest).toMat(TestSink[(Byte, ByteString)]())(Keep.both).run()

    subHandlersProbe.sendNext(subProtocolSet1)

    forAll(Gen.asciiStr) { s: String =>
      sub.request(1)
      pub.sendNext((1: Byte) -> ByteString(s))
      sub.expectNext((1: Byte, ByteString("echo1") ++ ByteString(s)))
    }

    subHandlersProbe.sendNext(Nil)

    forAll(Gen.asciiStr) { s: String =>
      sub.request(1)
      pub.sendNext((1: Byte) -> ByteString(s))
      sub.expectNoMessage()
    }
  }

  private def echo1 = {
    val (queue, source) = Source.queue[ByteString](128).map(ByteString("echo1") ++ _).preMaterialize()
    val sink = Sink.foreach[ByteString](queue.offer)

    source -> sink
  }

  private def echo2 = {
    val (queue, source) = Source.queue[ByteString](128).map(ByteString("echo2") ++ _).preMaterialize()
    val sink = Sink.foreach[ByteString](queue.offer)

    source -> sink
  }
}
