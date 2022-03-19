package co.topl.networking

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.Materializer
import akka.stream.scaladsl.{Sink, Source}
import cats.{Monad, Order}
import cats.effect.kernel.Sync
import cats.implicits._
import co.topl.models.TypedIdentifier
import co.topl.networking.p2p._
import co.topl.networking.typedprotocols.{TypedProtocol, TypedProtocolInstance}
import scodec.bits.ByteVector
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.networking.typedprotocols.blockchain.BlockchainProtocols
import co.topl.networking.typedprotocols.blockchain.BlockchainProtocols.NotificationProtocols

import java.net.InetSocketAddress
import scala.concurrent.Promise

trait BlockchainProtocolHandlers[F[_]] {
  def blockAdoptionNotificationClientSink(connectedPeer:   ConnectedPeer): F[Sink[TypedIdentifier, NotUsed]]
  def blockAdoptionNotificationServerSource(connectedPeer: ConnectedPeer): F[Source[TypedIdentifier, NotUsed]]
}

object BlockchainProtocolHandlers {

  val multiplexerCodec =
    MultiplexerCodecBuilder(Map.empty, Map.empty)
      .withEncoder[TypedProtocol.CommonMessages.Start.type](1: Byte)(_ => ByteVector.empty)
      .withDecoder(1: Byte)(_ => TypedProtocol.CommonMessages.Start)
      .withEncoder[TypedProtocol.CommonMessages.Push[TypedIdentifier]](4: Byte)(push => push.data.transmittableBytes)
      .withDecoder(4: Byte)(data => TypedProtocol.CommonMessages.Push(data.decodeTransmitted[TypedIdentifier]))
      .withEncoder[TypedProtocol.CommonMessages.Done.type](5: Byte)(_ => ByteVector.empty)
      .withDecoder(5: Byte)(_ => TypedProtocol.CommonMessages.Done)
      .multiplexerCodec

  def standardProtocolSet[F[_]](
    handlers:      BlockchainProtocolHandlers[F],
    connectedPeer: ConnectedPeer,
    localPeer:     LocalPeer
  )(implicit
    syncF:        Sync[F],
    monadF:       Monad[F],
    materializer: Materializer
  ): F[List[MultiplexedTypedSubHandler[F]]] = {

    val isAddressLeader = Order[InetSocketAddress].gt(localPeer.localAddress, connectedPeer.remoteAddress)
    Sync[F].defer(
      (
        handlers
          .blockAdoptionNotificationClientSink(connectedPeer)
          .flatMap { sink =>
            val queue = Source.queue(128).to(sink).run()
            val transitions = new NotificationProtocols.BlockAdoption.StateTransitionsClient[F](
              queue.offer(_).pure[F].void
            ) // TODO: Handle failures
            import transitions._
            TypedProtocolInstance(Parties.B)
              .withTransition(startNoneBusy)
              .withTransition(pushBusyBusy)
              .withTransition(doneBusyDone)
              .applier(TypedProtocol.CommonStates.None)
              .map(applier =>
                MultiplexedTypedSubHandler(
                  if (isAddressLeader) 1: Byte else 2: Byte,
                  if (isAddressLeader) Parties.B else Parties.A,
                  applier,
                  Source.single(OutboundMessage(TypedProtocol.CommonMessages.Start)).concat(Source.never),
                  multiplexerCodec
                )
              )
          },
        Sync[F]
          .delay {
            val promise = Promise[Unit]()
            val transitions = new NotificationProtocols.BlockAdoption.StateTransitionsServer[F](() =>
              Sync[F].delay(promise.success(())).void
            )
            transitions -> promise
          }
          .flatMap { case (transitions, clientSignalPromise) =>
            (
              handlers.blockAdoptionNotificationServerSource(connectedPeer), {
                import transitions._
                TypedProtocolInstance(Parties.A)
                  .withTransition(startNoneBusy)
                  .withTransition(pushBusyBusy)
                  .withTransition(doneBusyDone)
                  .applier(TypedProtocol.CommonStates.None)
              }
            )
              .mapN((source, applier) =>
                MultiplexedTypedSubHandler(
                  if (isAddressLeader) 2: Byte else 1: Byte,
                  if (isAddressLeader) Parties.A else Parties.B,
                  applier,
                  Source
                    .future(clientSignalPromise.future)
                    .flatMapConcat(_ => source.map(id => OutboundMessage(TypedProtocol.CommonMessages.Push(id)))),
                  multiplexerCodec
                )
              )
          }
      )
        .mapN((subHandler1, subHandler2) => List(subHandler1, subHandler2))
    )
  }
}
