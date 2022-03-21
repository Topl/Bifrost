package co.topl.networking

import akka.NotUsed
import akka.stream.scaladsl.{Sink, Source}
import akka.stream.{Materializer, QueueOfferResult}
import cats.effect.kernel.Sync
import cats.implicits._
import cats.{Applicative, Monad, MonadThrow}
import co.topl.codecs.bytes.scodecs.valuetypes._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.Transmittable
import co.topl.models.TypedIdentifier
import co.topl.networking.p2p._
import co.topl.networking.typedprotocols.blockchain.BlockchainProtocols.NotificationProtocols
import co.topl.networking.typedprotocols.{TypedProtocol, TypedProtocolInstance}
import scodec.bits.BitVector
import scodec.{Attempt, Codec, DecodeResult, Err}

import scala.concurrent.Promise

trait BlockchainProtocolHandlers[F[_]] {
  def blockAdoptionNotificationClientSink(connectedPeer:   ConnectedPeer): F[Sink[TypedIdentifier, NotUsed]]
  def blockAdoptionNotificationServerSource(connectedPeer: ConnectedPeer): F[Source[TypedIdentifier, NotUsed]]
}

object BlockchainProtocolHandlers {

  implicit val commonMessagesStartTransmittable: Transmittable[TypedProtocol.CommonMessages.Start.type] =
    Transmittable.instanceFromCodec(emptyCodec(TypedProtocol.CommonMessages.Start))

  implicit def commonMessagesPushTransmittable[T: Transmittable]: Transmittable[TypedProtocol.CommonMessages.Push[T]] =
    Transmittable.instanceFromCodec(
      Codec[TypedProtocol.CommonMessages.Push[T]](
        (p: TypedProtocol.CommonMessages.Push[T]) =>
          Attempt.successful(Transmittable[T].transmittableBytes(p.data)).map(_.toBitVector),
        (p: BitVector) =>
          Attempt.fromEither(
            Transmittable[T]
              .fromTransmittableBytes(p.toByteVector)
              .leftMap(Err(_))
              .map(DecodeResult(_, BitVector.empty).map(TypedProtocol.CommonMessages.Push(_)))
          )
      )
    )

  implicit val commonMessagesDoneTransmittable: Transmittable[TypedProtocol.CommonMessages.Done.type] =
    Transmittable.instanceFromCodec(emptyCodec(TypedProtocol.CommonMessages.Done))

  val multiplexerCodec =
    MultiplexerCodecBuilder(Map.empty, Map.empty)
      .withCodec[TypedProtocol.CommonMessages.Start.type](1: Byte)
      .withCodec[TypedProtocol.CommonMessages.Push[TypedIdentifier]](4: Byte)
      .withCodec[TypedProtocol.CommonMessages.Done.type](5: Byte)
      .multiplexerCodec

  def standardProtocolSet[F[_]: MonadThrow](
    handlers:         BlockchainProtocolHandlers[F],
    connectedPeer:    ConnectedPeer,
    connectionLeader: ConnectionLeader,
    localPeer:        LocalPeer
  )(implicit
    syncF:        Sync[F],
    monadF:       Monad[F],
    materializer: Materializer
  ): F[List[MultiplexedTypedSubHandler[F, _]]] = {
    val isAddressLeader = connectionLeader == ConnectionLeaders.Local
    Sync[F].defer(
      (
        Sync[F]
          .delay {
            val promise = Promise[Unit]()
            val transitions = new NotificationProtocols.BlockAdoption.StateTransitionsServer[F](() =>
              Sync[F].delay(promise.success(())).void
            )
            transitions -> promise
          }
          .flatMap { case (transitions, clientSignalPromise) =>
            import transitions._
            handlers
              .blockAdoptionNotificationServerSource(connectedPeer)
              .tupleRight(
                TypedProtocolInstance(Parties.A)
                  .withTransition(startNoneBusy)
                  .withTransition(pushBusyBusy)
                  .withTransition(doneBusyDone)
              )
              .map { case (source, instance) =>
                MultiplexedTypedSubHandler(
                  if (isAddressLeader) 1: Byte else 2: Byte,
                  instance,
                  TypedProtocol.CommonStates.None,
                  Source
                    .future(clientSignalPromise.future)
                    .flatMapConcat(_ => source.map(id => OutboundMessage(TypedProtocol.CommonMessages.Push(id)))),
                  multiplexerCodec
                )
              }
          },
        handlers
          .blockAdoptionNotificationClientSink(connectedPeer)
          .map { sink =>
            val queue = Source.queue[TypedIdentifier](128).to(sink).run()
            val transitions = new NotificationProtocols.BlockAdoption.StateTransitionsClient[F](id =>
              (queue.offer(id) match {
                case QueueOfferResult.Enqueued =>
                  Applicative[F].unit
                case QueueOfferResult.Dropped =>
                  MonadThrow[F].raiseError(new IllegalStateException("Downstream too slow"))
                case QueueOfferResult.QueueClosed =>
                  MonadThrow[F].raiseError(new IllegalStateException("Queue closed"))
                case QueueOfferResult.Failure(e) =>
                  MonadThrow[F].raiseError(e)
              })
            ) // TODO: Handle failures
            import transitions._
            val instance = TypedProtocolInstance(Parties.B)
              .withTransition(startNoneBusy)
              .withTransition(pushBusyBusy)
              .withTransition(doneBusyDone)

            MultiplexedTypedSubHandler(
              if (isAddressLeader) 2: Byte else 1: Byte,
              instance,
              TypedProtocol.CommonStates.None,
              Source.single(OutboundMessage(TypedProtocol.CommonMessages.Start)).concat(Source.never),
              multiplexerCodec
            )
          }
      )
        .mapN((subHandler1, subHandler2) => List(subHandler1, subHandler2))
    )
  }
}
