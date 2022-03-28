package co.topl.networking.blockchain

import akka.NotUsed
import akka.stream.Materializer
import akka.stream.scaladsl.{BroadcastHub, Flow, Keep, Sink, Source}
import akka.util.ByteString
import cats.effect.kernel.Sync
import cats.effect.{Async, Ref}
import cats.implicits._
import co.topl.catsakka._
import co.topl.models.{BlockBodyV2, BlockHeaderV2, Transaction, TypedIdentifier}
import co.topl.networking.TypedProtocolSetFactory.implicits._
import co.topl.networking._
import co.topl.networking.blockchain.BlockchainMultiplexerCodecs.multiplexerCodec
import co.topl.networking.blockchain.NetworkTypeTags._
import co.topl.networking.p2p.{ConnectedPeer, ConnectionLeader}
import co.topl.networking.typedprotocols.{
  NotificationProtocol,
  RequestResponseProtocol,
  TypedProtocol,
  TypedProtocolInstance
}
import org.typelevel.log4cats.Logger

import scala.concurrent.Promise

/**
 * Produces a function which accepts a (connectedPeer, connectionLeader) and emits an Akka Stream Flow.  The flow performs
 * the inbound and outbound communications to a specific peer.
 *
 * Specifically, the Flow runs a Multiplexer which serves several Blockchain Typed Protocols.  The Typed Protocols
 * are instantiated using the methods from the provided `BlockchainPeerServer`.
 */
object BlockchainPeerConnectionFlowFactory {

  def make[F[_]: Async: Logger: FToFuture](peerServer: BlockchainPeerServer[F])(implicit
    materializer:                                      Materializer
  ): (ConnectedPeer, ConnectionLeader) => F[Flow[ByteString, ByteString, BlockchainPeerClient[F]]] =
    createFactory(peerServer).multiplexed

  private def createFactory[F[_]: Async: Logger: FToFuture](protocolServer: BlockchainPeerServer[F])(implicit
    materializer:                                                           Materializer
  ): TypedProtocolSetFactory[F, BlockchainPeerClient[F]] = {
    val blockAdoptionRecipF =
      notificationReciprocated(BlockchainProtocols.BlockAdoption, protocolServer.localBlockAdoptions, 1: Byte, 2: Byte)

    val headerRecipF =
      requestResponseReciprocated(BlockchainProtocols.Header, protocolServer.getLocalHeader, 3: Byte, 4: Byte)

    val bodyRecipF =
      requestResponseReciprocated(BlockchainProtocols.Body, protocolServer.getLocalBody, 5: Byte, 6: Byte)

    val transactionRecipF =
      requestResponseReciprocated(BlockchainProtocols.Transaction, protocolServer.getLocalTransaction, 7: Byte, 8: Byte)

    (connectedPeer: ConnectedPeer, connectionLeader: ConnectionLeader) =>
      for {
        (adoptionTypedSubHandlers, remoteBlockIdsSource)           <- blockAdoptionRecipF.ap(connectionLeader.pure[F])
        (headerTypedSubHandlers, headerReceivedCallback)           <- headerRecipF.ap(connectionLeader.pure[F])
        (bodyTypedSubHandlers, bodyReceivedCallback)               <- bodyRecipF.ap(connectionLeader.pure[F])
        (transactionTypedSubHandlers, transactionReceivedCallback) <- transactionRecipF.ap(connectionLeader.pure[F])
        blockchainProtocolClient = new BlockchainPeerClient[F] {
          def remotePeerAdoptions: F[Source[TypedIdentifier, NotUsed]] = Sync[F].delay(remoteBlockIdsSource)
          def getRemoteHeader(id: TypedIdentifier): F[Option[BlockHeaderV2]] = headerReceivedCallback(id)
          def getRemoteBody(id: TypedIdentifier): F[Option[BlockBodyV2]] = bodyReceivedCallback(id)
          def getRemoteTransaction(id: TypedIdentifier): F[Option[Transaction]] = transactionReceivedCallback(id)
        }
        subHandlers =
          adoptionTypedSubHandlers ++ headerTypedSubHandlers ++ bodyTypedSubHandlers ++ transactionTypedSubHandlers
      } yield subHandlers -> blockchainProtocolClient
  }

  private def notificationReciprocated[F[_]: Async, T](
    protocol:              NotificationProtocol[T],
    notifications:         F[Source[T, NotUsed]],
    byteA:                 Byte,
    byteB:                 Byte
  )(implicit materializer: Materializer, tPushTypeTag: NetworkTypeTag[TypedProtocol.CommonMessages.Push[T]]) =
    (notificationServer[F, T](protocol, notifications), notificationClient[F, T](protocol)).tupled
      .map { case (f1, (f2, source)) =>
        (connectionLeader: ConnectionLeader) =>
          (ReciprocatedTypedSubHandler(f1, f2, byteA, byteB).handlers(connectionLeader), source)
      }

  private def requestResponseReciprocated[F[_]: Async, T](
    protocol:              RequestResponseProtocol[TypedIdentifier, T],
    fetch:                 TypedIdentifier => F[Option[T]],
    byteA:                 Byte,
    byteB:                 Byte
  )(implicit materializer: Materializer, tResponseTypeTag: NetworkTypeTag[TypedProtocol.CommonMessages.Response[T]]) =
    (requestResponseServer(protocol, fetch), requestResponseClient(protocol)).tupled
      .map { case (f1, (f2, callback)) =>
        (connectionLeader: ConnectionLeader) =>
          (ReciprocatedTypedSubHandler(f1, f2, byteA, byteB).handlers(connectionLeader), callback)
      }

  private def notificationServer[F[_]: Async, T](
    protocol:              NotificationProtocol[T],
    notifications:         F[Source[T, NotUsed]]
  )(implicit tPushTypeTag: NetworkTypeTag[TypedProtocol.CommonMessages.Push[T]]) =
    for {
      clientSignalPromise <- Sync[F].delay(Promise[Unit]())
      protocolInstance <- Sync[F].delay {
        val transitions =
          new protocol.StateTransitionsServer[F](() => Sync[F].delay(clientSignalPromise.success(())).void)
        import transitions._
        TypedProtocolInstance(Parties.A)
          .withTransition(startNoneBusy)
          .withTransition(pushBusyBusy)
          .withTransition(doneBusyDone)
      }
      notifications_ <- notifications
    } yield (
      (sessionId: Byte) =>
        TypedSubHandler(
          sessionId,
          instance = protocolInstance,
          initialState = TypedProtocol.CommonStates.None,
          outboundMessages = Source
            .future(clientSignalPromise.future)
            .flatMapConcat(_ =>
              notifications_
                .map(TypedProtocol.CommonMessages.Push(_))
                .map(OutboundMessage(_))
            ),
          codec = multiplexerCodec
        )
    )

  private def notificationClient[F[_]: Async, T](
    protocol:              NotificationProtocol[T]
  )(implicit materializer: Materializer, tPushTypeTag: NetworkTypeTag[TypedProtocol.CommonMessages.Push[T]]) =
    for {
      (((offerF, completeF), demandSignaled), source) <- Source
        .backpressuredQueue[F, T](16)
        // A Notification Client must send a `Start` message to the server before it will start
        // pushing notifications. We can signal this message to the server once the returned Source here requests
        // data for the first time
        .viaMat(OnFirstDemandFlow.apply)(Keep.both)
        .toMat(BroadcastHub.sink)(Keep.both)
        .liftTo[F]
      instance <- Sync[F].delay {
        val transitions =
          new protocol.StateTransitionsClient[F](offerF)
        import transitions._
        TypedProtocolInstance(Parties.B)
          .withTransition(startNoneBusy)
          .withTransition(pushBusyBusy)
          .withTransition(doneBusyDone)
      }
      subHandler = (sessionId: Byte) =>
        TypedSubHandler(
          sessionId,
          instance,
          TypedProtocol.CommonStates.None,
          Source
            .future(demandSignaled)
            .map(_ => OutboundMessage(TypedProtocol.CommonMessages.Start))
            .concat(Source.never),
          multiplexerCodec
        )
    } yield (subHandler, source)

  private def requestResponseServer[F[_]: Async, T](
    protocol:              RequestResponseProtocol[TypedIdentifier, T],
    fetch:                 TypedIdentifier => F[Option[T]]
  )(implicit materializer: Materializer, tResponseTypeTag: NetworkTypeTag[TypedProtocol.CommonMessages.Response[T]]) =
    for {
      ((offerResponseF, completeResponsesF), responsesSource) <- Sync[F].delay(
        Source.backpressuredQueue[F, Option[T]](128).preMaterialize()
      )
      transitions = new protocol.ServerStateTransitions[F](fetch(_).flatMap(offerResponseF))
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
          .concat(responsesSource.map(TypedProtocol.CommonMessages.Response(_)).map(OutboundMessage(_)))
          .alsoTo(Sink.onComplete(result => completeResponsesF(result.failed.toOption))),
        multiplexerCodec
      )

  private def requestResponseClient[F[_]: Async, T](
    protocol:              RequestResponseProtocol[TypedIdentifier, T]
  )(implicit materializer: Materializer, tResponseTypeTag: NetworkTypeTag[TypedProtocol.CommonMessages.Response[T]]) =
    for {
      // Stores a reference to a Promise that is created for each inbound request.  The state transition implementation
      // will complete the promise.
      currentResponsePromiseRef <- Sync[F].defer(Ref.of[F, Option[Promise[Option[T]]]](none))
      serverSentGoPromise = Promise[Unit]()
      transitions =
        new protocol.ClientStateTransitions[F](
          r =>
            currentResponsePromiseRef.update { current =>
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
      ((offerOutboundMessage, completeOutboundMessages), outboundMessagesSource) <- Source
        .backpressuredQueue[F, OutboundMessage](8)
        .toMat(BroadcastHub.sink)(Keep.both)
        .liftTo[F]
      clientCallback = (id: TypedIdentifier) =>
        offerOutboundMessage(OutboundMessage(TypedProtocol.CommonMessages.Get(id))) >>
        Sync[F]
          .delay(Promise[Option[T]]())
          .flatMap(promise =>
            currentResponsePromiseRef
              .set(promise.some)
              .productR(Async[F].fromFuture(promise.future.pure[F]))
          )
      subHandler = (sessionId: Byte) =>
        TypedSubHandler(
          sessionId,
          instance,
          TypedProtocol.CommonStates.None,
          outboundMessagesSource
            .alsoTo(Sink.onComplete(result => completeOutboundMessages(result.failed.toOption))),
          multiplexerCodec
        )
    } yield (subHandler, clientCallback)

}
