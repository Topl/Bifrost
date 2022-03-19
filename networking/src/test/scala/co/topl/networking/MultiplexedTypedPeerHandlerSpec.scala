package co.topl.networking

import akka.NotUsed
import akka.actor.ActorSystem
import akka.actor.typed.scaladsl.adapter._
import akka.stream.scaladsl.{Keep, Sink, Source}
import akka.stream.testkit.scaladsl.{TestSink, TestSource}
import akka.testkit.TestKitBase
import akka.util.ByteString
import cats.effect._
import cats._
import cats.arrow.FunctionK
import cats.effect.unsafe.IORuntime
import cats.implicits._
import co.topl.interpreters.AkkaCatsRuntime
import co.topl.models.{Bytes, TypedBytes, TypedIdentifier}
import co.topl.networking.multiplexer.{MessageParserFramer, MessageSerializerFramer}
import co.topl.networking.p2p.{ConnectedPeer, LocalPeer}
import org.scalamock.scalatest.MockFactory
import org.scalatest.{BeforeAndAfterAll, EitherValues, OptionValues}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

import java.net.InetSocketAddress
import scala.concurrent.Future
import scala.concurrent.duration._

class MultiplexedTypedPeerHandlerSpec
    extends AnyFlatSpec
    with BeforeAndAfterAll
    with MockFactory
    with Matchers
    with ScalaCheckPropertyChecks
    with ScalaCheckDrivenPropertyChecks
    with EitherValues
    with OptionValues
    with TestKitBase {

  behavior of "MultiplexedTypedPeerHandler"

  implicit lazy val system: ActorSystem = ActorSystem()
  implicit val runtime: IORuntime = AkkaCatsRuntime(system.toTyped).runtime

  implicit val fToFuture: ~>[F, Future] = FunctionK.liftFunction[F, Future](_.unsafeToFuture())

  type F[A] = IO[A]

  it should "produce a multiplexer flow for typed protocols" in {
    val blockchainHandlers =
      new BlockchainProtocolHandlers[F] {
        def blockAdoptionNotificationClientSink(connectedPeer: ConnectedPeer): F[Sink[TypedIdentifier, NotUsed]] =
          Sink
            .foreach[TypedIdentifier](id => println(s"Received id=$id from $connectedPeer"))
            .mapMaterializedValue(_ => NotUsed: NotUsed)
            .pure[F]

        def blockAdoptionNotificationServerSource(connectedPeer: ConnectedPeer): F[Source[TypedIdentifier, NotUsed]] =
          Source
            .tick(0.seconds, 1.seconds, ())
            .map(_ => TypedBytes(1: Byte, Bytes.fill(32)(0: Byte)))
            .mapMaterializedValue(_ => NotUsed: NotUsed)
            .pure[F]
      }

    val localPeer = LocalPeer(InetSocketAddress.createUnresolved("localhost", 9005))
    val connectedPeer = ConnectedPeer(InetSocketAddress.createUnresolved("localhost", 9004))

    val multiplexedTypedPeerHandler: MultiplexedTypedPeerHandler[F] =
      connectedPeer =>
        BlockchainProtocolHandlers
          .standardProtocolSet[F](blockchainHandlers, connectedPeer, localPeer)
          .map(Source.single(_).concat(Source.never))

    val flow = multiplexedTypedPeerHandler.multiplexed(connectedPeer).unsafeRunSync()

    val underTest =
      MessageSerializerFramer().via(flow).via(MessageParserFramer())

    val (pub, sub) =
      TestSource.probe[(Byte, ByteString)].via(underTest).toMat(TestSink[(Byte, ByteString)]())(Keep.both).run()

    sub.request(1)
    pub.expectRequest()
    pub.sendNext((1: Byte, ByteString(1: Byte) ++ multiplexer.intToBytestring(0)))
    val foo = sub.expectNext()

    println(foo)

  }

}
