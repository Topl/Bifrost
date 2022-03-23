package co.topl.networking.blockchain

import akka.NotUsed
import akka.stream.scaladsl.{BroadcastHub, Flow, Keep, Source}
import akka.stream.{BoundedSourceQueue, Materializer, QueueOfferResult}
import akka.util.ByteString
import cats.effect.kernel.Sync
import cats.effect.{Async, Ref}
import cats.implicits._
import cats.{~>, Applicative, MonadThrow}
import co.topl.models.{BlockBodyV2, BlockHeaderV2, TypedBytes, TypedIdentifier}
import co.topl.networking.blockchain.BlockchainMultiplexerCodecs.multiplexerCodec
import co.topl.networking.blockchain.NetworkTypeTags._
import co.topl.networking.p2p.{ConnectedPeer, ConnectionLeader, ConnectionLeaders, LocalPeer}
import co.topl.networking.typedprotocols.{StateTransition, TypedProtocol, TypedProtocolInstance}
import co.topl.networking.{MultiplexedTypedPeerHandler, MultiplexedTypedSubHandler, OutboundMessage, Parties}

import scala.concurrent.{Future, Promise}

object BlockchainPeerConnection {

  def make[F[_]: Async: *[_] ~> Future](
    connectedPeer:    ConnectedPeer,
    connectionLeader: ConnectionLeader,
    localPeer:        LocalPeer
  )(
    protocolServer:        BlockchainProtocolServer[F]
  )(implicit materializer: Materializer): F[Flow[ByteString, ByteString, BlockchainProtocolClient[F]]] =
    for {
      isConnectionLeader            <- (connectionLeader == ConnectionLeaders.Local).pure[F]
      adoptionTypedServerSubHandler <- blockAdoptionNotificationServer(protocolServer, isConnectionLeader)
      headerTypedServerSubHandler   <- blockHeaderRequestResponseServer(protocolServer, isConnectionLeader)
      bodyTypedServerSubHandler     <- blockBodyRequestResponseServer(protocolServer, isConnectionLeader)
      (adoptionTypedClientSubHandler, remoteBlockIdsSource) <- blockAdoptionNotificationClient(isConnectionLeader)
      (headerTypedClientSubHandler, headerReceivedCallback) <- blockHeaderRequestResponseClient(isConnectionLeader)
      (bodyTypedClientSubHandler, bodyReceivedCallback)     <- blockBodyRequestResponseClient(isConnectionLeader)
      blockchainProtocolClient = new BlockchainProtocolClient[F] {
        def remotePeerAdoptions: F[Source[TypedIdentifier, NotUsed]] = Sync[F].delay(remoteBlockIdsSource)
        def getRemoteHeader(id: TypedIdentifier): F[Option[BlockHeaderV2]] = headerReceivedCallback(id)
        def getRemoteBody(id: TypedIdentifier): F[Option[BlockBodyV2]] = bodyReceivedCallback(id)
      }
      multiplexedTypedPeerHandler = new MultiplexedTypedPeerHandler[F, BlockchainProtocolClient[F]] {

        def protocolsForPeer(
          connectedPeer:    ConnectedPeer,
          connectionLeader: ConnectionLeader
        ): F[(List[MultiplexedTypedSubHandler[F, _]], BlockchainProtocolClient[F])] =
          Sync[F].delay(
            List[MultiplexedTypedSubHandler[F, _]](
              adoptionTypedServerSubHandler,
              headerTypedServerSubHandler,
              bodyTypedServerSubHandler,
              adoptionTypedClientSubHandler,
              headerTypedClientSubHandler,
              bodyTypedClientSubHandler
            ) -> blockchainProtocolClient
          )
      }
      flow <- multiplexedTypedPeerHandler.multiplexed(connectedPeer, connectionLeader)
    } yield flow

  private def blockAdoptionNotificationServer[F[_]: Async](
    server:                BlockchainProtocolServer[F],
    isConnectionLeader:    Boolean
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
            MultiplexedTypedSubHandler(
              if (isConnectionLeader) 1: Byte else 2: Byte,
              instance,
              TypedProtocol.CommonStates.None,
              Source
                .future(clientSignalPromise.future)
                .flatMapConcat(_ =>
                  source.map(id =>
                    OutboundMessage(
                      TypedProtocol.CommonMessages.Push(id)
                    )
                  )
                ),
              multiplexerCodec
            )
          }
      }

  private def blockHeaderRequestResponseServer[F[_]: Async](
    server:                BlockchainProtocolServer[F],
    isConnectionLeader:    Boolean
  )(implicit materializer: Materializer) =
    for {
      (responsesQueue, responsesSource) <- Sync[F].delay(Source.queue[Option[BlockHeaderV2]](128).preMaterialize())
      transitions = new BlockchainProtocols.Header.ServerStateTransitions[F](id =>
        server
          .getLocalHeader(id)
          .flatMap(headerOpt => offerToQueue[F, Option[BlockHeaderV2]](responsesQueue, headerOpt))
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
      if (isConnectionLeader) 3: Byte else 4: Byte,
      instance,
      TypedProtocol.CommonStates.None,
      Source
        .single(OutboundMessage(TypedProtocol.CommonMessages.Start))
        .concat(responsesSource.map(TypedProtocol.CommonMessages.Response(_)).map(OutboundMessage(_))),
      multiplexerCodec
    )

  private def blockBodyRequestResponseServer[F[_]: Async](
    server:                BlockchainProtocolServer[F],
    isConnectionLeader:    Boolean
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
    } yield MultiplexedTypedSubHandler(
      if (isConnectionLeader) 5: Byte else 6: Byte,
      instance,
      TypedProtocol.CommonStates.None,
      Source
        .single(OutboundMessage(TypedProtocol.CommonMessages.Start))
        .concat(responsesSource.map(TypedProtocol.CommonMessages.Response(_)).map(OutboundMessage(_))),
      multiplexerCodec
    )

  private def blockAdoptionNotificationClient[F[_]: Async](
    isConnectionLeader:    Boolean
  )(implicit materializer: Materializer) =
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
        MultiplexedTypedSubHandler(
          if (isConnectionLeader) 2: Byte else 1: Byte,
          instance,
          TypedProtocol.CommonStates.None,
          Source.single(OutboundMessage(TypedProtocol.CommonMessages.Start)).concat(Source.never),
          multiplexerCodec
        )
      subHandler -> source
    }

  private def blockHeaderRequestResponseClient[F[_]: Async](
    isConnectionLeader: Boolean
  )(implicit
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
      subHandler = MultiplexedTypedSubHandler(
        if (isConnectionLeader) 4: Byte else 3: Byte,
        instance,
        TypedProtocol.CommonStates.None,
        outboundMessagesSource,
        multiplexerCodec
      )
    } yield (subHandler, clientCallback)

  private def blockBodyRequestResponseClient[F[_]: Async](
    isConnectionLeader: Boolean
  )(implicit
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
      subHandler = MultiplexedTypedSubHandler(
        if (isConnectionLeader) 6: Byte else 5: Byte,
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
