package co.topl.networking

import akka.NotUsed
import akka.stream.scaladsl.{Sink, Source}
import akka.stream.{BoundedSourceQueue, Materializer, QueueOfferResult}
import cats.effect.kernel.Sync
import cats.implicits._
import cats.{Applicative, Monad, MonadThrow}
import co.topl.codecs.bytes.scodecs.valuetypes._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.Transmittable
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.models._
import co.topl.networking.p2p._
import co.topl.networking.typedprotocols.blockchain.BlockchainProtocols
import co.topl.networking.typedprotocols.blockchain.BlockchainProtocols.NotificationProtocols
import co.topl.networking.typedprotocols.{TypedProtocol, TypedProtocolInstance}
import scodec.bits.BitVector
import scodec.{Attempt, Codec, DecodeResult, Decoder, Encoder, Err}

import scala.concurrent.Promise

trait BlockchainProtocolHandlers[F[_]] {
  def blockAdoptionNotificationClientSink(connectedPeer:   ConnectedPeer): F[Sink[TypedIdentifier, NotUsed]]
  def blockAdoptionNotificationServerSource(connectedPeer: ConnectedPeer): F[Source[TypedIdentifier, NotUsed]]
  def getLocalBlockHeader(connectedPeer:                   ConnectedPeer)(id: TypedIdentifier): F[Option[BlockHeaderV2]]

  def blockHeaderRequestResponses(
    connectedPeer: ConnectedPeer
  ): F[(Source[TypedIdentifier, NotUsed], Sink[Option[BlockHeaderV2], NotUsed])]
}

object BlockchainProtocolHandlers {

  implicit val commonMessagesStartTransmittable: Transmittable[TypedProtocol.CommonMessages.Start.type] =
    Transmittable.instanceFromCodec(emptyCodec(TypedProtocol.CommonMessages.Start))

  implicit def commonMessagesGetTransmittable[Query: Transmittable]
    : Transmittable[TypedProtocol.CommonMessages.Get[Query]] =
    Transmittable.instanceFromCodec[TypedProtocol.CommonMessages.Get[Query]](
      Codec[TypedProtocol.CommonMessages.Get[Query]](
        (p: TypedProtocol.CommonMessages.Get[Query]) =>
          Attempt.successful(Transmittable[Query].transmittableBytes(p.query)).map(_.toBitVector),
        (p: BitVector) =>
          Attempt.fromEither(
            Transmittable[Query]
              .fromTransmittableBytes(p.toByteVector)
              .leftMap(Err(_))
              .map(DecodeResult(_, BitVector.empty).map(TypedProtocol.CommonMessages.Get(_)))
          )
      )
    )

  implicit def commonMessagesResponseTransmittable[T: Transmittable]
    : Transmittable[TypedProtocol.CommonMessages.Response[T]] =
    Transmittable.instanceFromCodec[TypedProtocol.CommonMessages.Response[T]](
      Codec[TypedProtocol.CommonMessages.Response[T]](
        encoder = Encoder((p: TypedProtocol.CommonMessages.Response[T]) =>
          p.dataOpt match {
            case Some(data) =>
              Attempt
                .successful(Transmittable[T].transmittableBytes(data))
                .map(b => BitVector(1: Byte) ++ b.toBitVector)
            case _ =>
              Attempt.successful(BitVector(0: Byte))
          }
        ),
        decoder = Decoder((p: BitVector) =>
          (if (p.head)
             Attempt.fromEither(
               Transmittable[T]
                 .fromTransmittableBytes(p.tail.toByteVector)
                 .leftMap(Err(_))
                 .map(DecodeResult(_, BitVector.empty).map(t => TypedProtocol.CommonMessages.Response(t.some)))
             )
           else
             Attempt.successful(DecodeResult(TypedProtocol.CommonMessages.Response(none[T]), p.tail)))
        )
      )
    )

  implicit def commonMessagesPushTransmittable[T: Transmittable]: Transmittable[TypedProtocol.CommonMessages.Push[T]] =
    Transmittable.instanceFromCodec[TypedProtocol.CommonMessages.Push[T]](
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
      .withCodec[TypedProtocol.CommonMessages.Done.type](2: Byte)
      .withCodec[TypedProtocol.CommonMessages.Get[TypedIdentifier]](3: Byte)
      .withCodec[TypedProtocol.CommonMessages.Response[SlotData]](4: Byte)
      .withCodec[TypedProtocol.CommonMessages.Response[BlockHeaderV2]](5: Byte)
      .withCodec[TypedProtocol.CommonMessages.Response[List[TypedIdentifier]]](6: Byte)
      .withCodec[TypedProtocol.CommonMessages.Response[Transaction]](7: Byte)
      .withCodec[TypedProtocol.CommonMessages.Push[TypedIdentifier]](8: Byte)
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
        blockAdoptionNotificationServer[F](handlers, connectedPeer, connectionLeader, localPeer, isAddressLeader),
        blockAdoptionNotificationClient[F](
          handlers,
          connectedPeer,
          connectionLeader,
          localPeer,
          isAddressLeader
        ),
        blockHeaderRequestResponseServer[F](handlers, connectedPeer, connectionLeader, localPeer, isAddressLeader),
        blockHeaderRequestResponseClient[F](handlers, connectedPeer, connectionLeader, localPeer, isAddressLeader)
      ).mapN((h1, h2, h3, h4) => List(h1, h2, h3, h4))
    )
  }

  private def blockAdoptionNotificationServer[F[_]: MonadThrow](
    handlers:         BlockchainProtocolHandlers[F],
    connectedPeer:    ConnectedPeer,
    connectionLeader: ConnectionLeader,
    localPeer:        LocalPeer,
    isAddressLeader:  Boolean
  )(implicit
    syncF:        Sync[F],
    monadF:       Monad[F],
    materializer: Materializer
  ) =
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
      }

  private def blockAdoptionNotificationClient[F[_]: MonadThrow](
    handlers:         BlockchainProtocolHandlers[F],
    connectedPeer:    ConnectedPeer,
    connectionLeader: ConnectionLeader,
    localPeer:        LocalPeer,
    isAddressLeader:  Boolean
  )(implicit
    syncF:        Sync[F],
    monadF:       Monad[F],
    materializer: Materializer
  ) =
    handlers
      .blockAdoptionNotificationClientSink(connectedPeer)
      .map { sink =>
        val queue = Source.queue[TypedIdentifier](128).to(sink).run()
        val transitions =
          new NotificationProtocols.BlockAdoption.StateTransitionsClient[F](offerToQueue[F, TypedIdentifier](queue, _))
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

  private def blockHeaderRequestResponseServer[F[_]: MonadThrow: Sync](
    handlers:              BlockchainProtocolHandlers[F],
    connectedPeer:         ConnectedPeer,
    connectionLeader:      ConnectionLeader,
    localPeer:             LocalPeer,
    isAddressLeader:       Boolean
  )(implicit materializer: Materializer) =
    for {
      (queue, requestsSource) <- Sync[F].delay(Source.queue[Option[BlockHeaderV2]](128).preMaterialize())
      transitions = new BlockchainProtocols.RequestResponseProtocols.Header.ServerStateTransitions[F](
        handlers.getLocalBlockHeader(connectedPeer)(_).flatMap(offerToQueue[F, Option[BlockHeaderV2]](queue, _))
      )
      instance = {
        import transitions._
        TypedProtocolInstance(Parties.A)
          .withTransition(startNoneIdle)
          .withTransition(getIdleBusy)
          .withTransition(responseBusyIdle)
          .withTransition(doneIdleDone)
      }
    } yield MultiplexedTypedSubHandler(
      if (isAddressLeader) 3: Byte else 4: Byte,
      instance,
      TypedProtocol.CommonStates.None,
      Source
        .single(OutboundMessage(TypedProtocol.CommonMessages.Start))
        .concat(requestsSource.map(TypedProtocol.CommonMessages.Response(_)).map(OutboundMessage(_))),
      multiplexerCodec
    )

  private def blockHeaderRequestResponseClient[F[_]: MonadThrow: Sync](
    handlers:              BlockchainProtocolHandlers[F],
    connectedPeer:         ConnectedPeer,
    connectionLeader:      ConnectionLeader,
    localPeer:             LocalPeer,
    isAddressLeader:       Boolean
  )(implicit materializer: Materializer) =
    for {
      (requests, responses) <- handlers.blockHeaderRequestResponses(connectedPeer)
      queue = Source.queue[Option[BlockHeaderV2]](128).to(responses).run()
      promise = Promise[Unit]()
      transitions = new BlockchainProtocols.RequestResponseProtocols.Header.ClientStateTransitions[F](
        offerToQueue[F, Option[BlockHeaderV2]](queue, _),
        () => Sync[F].delay(promise.success(()))
      )
      instance = {
        import transitions._
        TypedProtocolInstance(Parties.B)
          .withTransition(startNoneIdle)
          .withTransition(getIdleBusy)
          .withTransition(responseBusyIdle)
          .withTransition(doneIdleDone)
      }
    } yield MultiplexedTypedSubHandler(
      if (isAddressLeader) 4: Byte else 3: Byte,
      instance,
      TypedProtocol.CommonStates.None,
      Source
        .future(promise.future)
        .flatMapConcat(_ => requests.map(TypedProtocol.CommonMessages.Get(_)).map(OutboundMessage(_))),
      multiplexerCodec
    )

  private def offerToQueue[F[_]: MonadThrow, T](queue: BoundedSourceQueue[T], data: T) =
    (queue.offer(data) match {
      case QueueOfferResult.Enqueued =>
        Applicative[F].unit
      case QueueOfferResult.Dropped =>
        MonadThrow[F].raiseError(new IllegalStateException("Downstream too slow"))
      case QueueOfferResult.QueueClosed =>
        MonadThrow[F].raiseError(new IllegalStateException("Queue closed"))
      case QueueOfferResult.Failure(e) =>
        MonadThrow[F].raiseError(e)
    }).void
}
