package co.topl.networking.blockchain

import akka.NotUsed
import akka.stream.scaladsl.{BroadcastHub, Flow, Keep, Source}
import akka.stream.{BoundedSourceQueue, Materializer, QueueOfferResult}
import akka.util.ByteString
import cats.effect.kernel.Sync
import cats.effect.{Async, Ref}
import cats.implicits._
import cats.{~>, Applicative, MonadThrow}
import co.topl.models.{BlockBodyV2, BlockHeaderV2, TypedIdentifier}
import co.topl.networking.TypedProtocolSetFactory.implicits._
import co.topl.networking._
import co.topl.networking.blockchain.BlockchainMultiplexerCodecs.multiplexerCodec
import co.topl.networking.blockchain.NetworkTypeTags._
import co.topl.networking.p2p.{ConnectedPeer, ConnectionLeader}
import co.topl.networking.typedprotocols.{TypedProtocol, TypedProtocolInstance}

import scala.concurrent.{Future, Promise}

/**
 * Produces a function which accepts a (connectedPeer, connectionLeader) and emits an Akka Stream Flow.  The flow performs
 * the inbound and outbound communications to a specific peer.
 *
 * Specifically, the Flow runs a Multiplexer which serves several Blockchain Typed Protocols.  The Typed Protocols
 * are instantiated using the methods from the provided `BlockchainPeerServer`.
 */
object BlockchainPeerConnectionFlowFactory {

  def make[F[_]: Async: *[_] ~> Future](peerServer: BlockchainPeerServer[F])(implicit
    materializer:                                   Materializer
  ): (ConnectedPeer, ConnectionLeader) => F[Flow[ByteString, ByteString, BlockchainPeerClient[F]]] =
    createFactory(peerServer).multiplexed

  private def createFactory[F[_]: Async: *[_] ~> Future](protocolServer: BlockchainPeerServer[F])(implicit
    materializer:                                                        Materializer
  ): TypedProtocolSetFactory[F, BlockchainPeerClient[F]] =
    (
      connectedPeer:    ConnectedPeer,
      connectionLeader: ConnectionLeader
    ) =>
      for {
        (adoptionTypedSubHandlers, remoteBlockIdsSource) <- (
          blockAdoptionNotificationServer(protocolServer),
          blockAdoptionNotificationClient
        ).tupled.map { case (f1, (f2, source)) =>
          ReciprocatedTypedSubHandler(f1, f2, 1: Byte, 2: Byte).handlers(connectionLeader) -> source
        }
        (headerTypedSubHandlers, headerReceivedCallback) <- (
          blockHeaderRequestResponseServer(protocolServer),
          blockHeaderRequestResponseClient
        ).tupled.map { case (f1, (f2, callback)) =>
          ReciprocatedTypedSubHandler(f1, f2, 3: Byte, 4: Byte).handlers(connectionLeader) -> callback
        }
        (bodyTypedSubHandlers, bodyReceivedCallback) <- (
          blockBodyRequestResponseServer(protocolServer),
          blockBodyRequestResponseClient
        ).tupled.map { case (f1, (f2, callback)) =>
          ReciprocatedTypedSubHandler(f1, f2, 5: Byte, 6: Byte).handlers(connectionLeader) -> callback
        }
        blockchainProtocolClient = new BlockchainPeerClient[F] {
          def remotePeerAdoptions: F[Source[TypedIdentifier, NotUsed]] = Sync[F].delay(remoteBlockIdsSource)
          def getRemoteHeader(id: TypedIdentifier): F[Option[BlockHeaderV2]] = headerReceivedCallback(id)
          def getRemoteBody(id: TypedIdentifier): F[Option[BlockBodyV2]] = bodyReceivedCallback(id)
        }
      } yield (adoptionTypedSubHandlers ++ headerTypedSubHandlers ++ bodyTypedSubHandlers) -> blockchainProtocolClient

  private def blockAdoptionNotificationServer[F[_]: Async](
    server:                BlockchainPeerServer[F]
  )(implicit materializer: Materializer) =
    Sync[F]
      .delay {
        val promise = Promise[Unit]()
        val transitions =
          new BlockchainProtocols.BlockAdoption.StateTransitionsServer[F](() => Sync[F].delay(promise.success(())).void)
        transitions -> promise
      }
      .flatMap { case (transitions, clientSignalPromise) =>
        import transitions._
        server.localBlockAdoptions
          .tupleRight(
            TypedProtocolInstance(Parties.A)
              .withTransition(startNoneBusy)
              .withTransition(pushBusyBusy)
              .withTransition(doneBusyDone)
          )
          .map { case (source, instance) =>
            (sessionId: Byte) =>
              TypedSubHandler(
                sessionId,
                instance = instance,
                initialState = TypedProtocol.CommonStates.None,
                outboundMessages = Source
                  .future(clientSignalPromise.future)
                  .flatMapConcat(_ => source.map(TypedProtocol.CommonMessages.Push(_)).map(OutboundMessage(_))),
                codec = multiplexerCodec
              )
          }
      }

  private def blockHeaderRequestResponseServer[F[_]: Async](
    server:                BlockchainPeerServer[F]
  )(implicit materializer: Materializer) =
    for {
      (responsesQueue, responsesSource) <- Sync[F].delay(Source.queue[Option[BlockHeaderV2]](128).preMaterialize())
      transitions = new BlockchainProtocols.Header.ServerStateTransitions[F](id =>
        server
          .getLocalHeader(id)
          .flatMap(offerToQueue(responsesQueue, _))
      )
      instance = {
        import transitions._
        TypedProtocolInstance(Parties.A)
          .withTransition(startNoneIdle)
          .withTransition(getIdleBusy)
          .withTransition(responseBusyIdle)
          .withTransition(doneIdleDone)
      }
    } yield (sessionId: Byte) =>
      TypedSubHandler(
        sessionId,
        instance,
        TypedProtocol.CommonStates.None,
        Source
          .single(OutboundMessage(TypedProtocol.CommonMessages.Start))
          .concat(responsesSource.map(TypedProtocol.CommonMessages.Response(_)).map(OutboundMessage(_))),
        multiplexerCodec
      )

  private def blockBodyRequestResponseServer[F[_]: Async](
    server:                BlockchainPeerServer[F]
  )(implicit materializer: Materializer) =
    for {
      (responsesQueue, responsesSource) <- Sync[F].delay(Source.queue[Option[BlockBodyV2]](128).preMaterialize())
      transitions = new BlockchainProtocols.Body.ServerStateTransitions[F](id =>
        server
          .getLocalBody(id)
          .flatMap(offerToQueue[F, Option[BlockBodyV2]](responsesQueue, _))
      )
      instance = {
        import transitions._
        TypedProtocolInstance(Parties.A)
          .withTransition(startNoneIdle)
          .withTransition(getIdleBusy)
          .withTransition(responseBusyIdle)
          .withTransition(doneIdleDone)
      }
    } yield (sessionId: Byte) =>
      TypedSubHandler(
        sessionId,
        instance,
        TypedProtocol.CommonStates.None,
        Source
          .single(OutboundMessage(TypedProtocol.CommonMessages.Start))
          .concat(responsesSource.map(TypedProtocol.CommonMessages.Response(_)).map(OutboundMessage(_))),
        multiplexerCodec
      )

  private def blockAdoptionNotificationClient[F[_]: Async](implicit materializer: Materializer) =
    Sync[F].delay {
      val (queue, source) = Source
        .queue[TypedIdentifier](128)
        .toMat(BroadcastHub.sink)(Keep.both)
        .run()
      val transitions =
        new BlockchainProtocols.BlockAdoption.StateTransitionsClient[F](offerToQueue[F, TypedIdentifier](queue, _))
      import transitions._
      val instance = TypedProtocolInstance(Parties.B)
        .withTransition(startNoneBusy)
        .withTransition(pushBusyBusy)
        .withTransition(doneBusyDone)

      val subHandler =
        (sessionId: Byte) =>
          TypedSubHandler(
            sessionId,
            instance,
            TypedProtocol.CommonStates.None,
            Source.single(OutboundMessage(TypedProtocol.CommonMessages.Start)).concat(Source.never),
            multiplexerCodec
          )
      subHandler -> source
    }

  private def blockHeaderRequestResponseClient[F[_]: Async](implicit
    materializer: Materializer
  ) =
    for {
      currentPromiseRef <- Ref.of[F, Option[Promise[Option[BlockHeaderV2]]]](none)
      serverSentGoPromise = Promise[Unit]()
      transitions: BlockchainProtocols.Header.ClientStateTransitions[F] =
        new BlockchainProtocols.Header.ClientStateTransitions[F](
          r =>
            currentPromiseRef.update { current =>
              current.foreach(_.success(r))
              None
            },
          () => Sync[F].delay(serverSentGoPromise.success(()))
        )
      instance = {
        import transitions._
        TypedProtocolInstance(Parties.B)
          .withTransition(startNoneIdle)
          .withTransition(getIdleBusy)
          .withTransition(responseBusyIdle)
          .withTransition(doneIdleDone)
      }
      (outboundMessagesQueue, outboundMessagesSource) = Source
        .queue[OutboundMessage](128)
        .toMat(BroadcastHub.sink)(Keep.both)
        .run()
      clientCallback = (id: TypedIdentifier) =>
        offerToQueue(outboundMessagesQueue, OutboundMessage(TypedProtocol.CommonMessages.Get(id))) >>
        Sync[F]
          .delay(Promise[Option[BlockHeaderV2]]())
          .flatMap(promise => currentPromiseRef.set(promise.some) >> Async[F].fromFuture(promise.future.pure[F]))
      subHandler = (sessionId: Byte) =>
        TypedSubHandler(
          sessionId,
          instance,
          TypedProtocol.CommonStates.None,
          outboundMessagesSource,
          multiplexerCodec
        )
    } yield (subHandler, clientCallback)

  private def blockBodyRequestResponseClient[F[_]: Async](implicit
    materializer: Materializer
  ) =
    for {
      currentPromiseRef <- Ref.of[F, Option[Promise[Option[BlockBodyV2]]]](none)
      serverSentGoPromise = Promise[Unit]()
      transitions: BlockchainProtocols.Body.ClientStateTransitions[F] =
        new BlockchainProtocols.Body.ClientStateTransitions[F](
          r =>
            currentPromiseRef.update { current =>
              current.foreach(_.success(r))
              None
            },
          () => Sync[F].delay(serverSentGoPromise.success(()))
        )
      instance = {
        import transitions._
        TypedProtocolInstance(Parties.B)
          .withTransition(startNoneIdle)
          .withTransition(getIdleBusy)
          .withTransition(responseBusyIdle)
          .withTransition(doneIdleDone)
      }
      (outboundMessagesQueue, outboundMessagesSource) = Source
        .queue[OutboundMessage](128)
        .toMat(BroadcastHub.sink)(Keep.both)
        .run()
      clientCallback = (id: TypedIdentifier) =>
        offerToQueue(outboundMessagesQueue, OutboundMessage(TypedProtocol.CommonMessages.Get(id))) >>
        Sync[F]
          .delay(Promise[Option[BlockBodyV2]]())
          .flatMap(promise => currentPromiseRef.set(promise.some) >> Async[F].fromFuture(promise.future.pure[F]))
      subHandler = (sessionId: Byte) =>
        TypedSubHandler(
          sessionId,
          instance,
          TypedProtocol.CommonStates.None,
          outboundMessagesSource,
          multiplexerCodec
        )
    } yield (subHandler, clientCallback)

  private def offerToQueue[F[_]: Async, T](queue: BoundedSourceQueue[T], data: T) =
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
