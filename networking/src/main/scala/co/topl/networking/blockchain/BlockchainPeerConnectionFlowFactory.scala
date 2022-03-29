package co.topl.networking.blockchain

import akka.NotUsed
import akka.stream.Materializer
import akka.stream.scaladsl.{BroadcastHub, Flow, Keep, Sink, Source}
import akka.util.ByteString
import cats.Show
import cats.data.NonEmptyChain
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
import co.topl.networking.typedprotocols.TypedProtocol.CommonStates
import co.topl.networking.typedprotocols.{
  NotificationProtocol,
  RequestResponseProtocol,
  TypedProtocol,
  TypedProtocolInstance
}
import co.topl.typeclasses.implicits._
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
          val remotePeerAdoptions: F[Source[TypedIdentifier, NotUsed]] = remoteBlockIdsSource.pure[F]
          def getRemoteHeader(id: TypedIdentifier): F[Option[BlockHeaderV2]] = headerReceivedCallback(id)
          def getRemoteBody(id: TypedIdentifier): F[Option[BlockBodyV2]] = bodyReceivedCallback(id)
          def getRemoteTransaction(id: TypedIdentifier): F[Option[Transaction]] = transactionReceivedCallback(id)
        }
        subHandlers =
          adoptionTypedSubHandlers ++ headerTypedSubHandlers ++ bodyTypedSubHandlers ++ transactionTypedSubHandlers
      } yield subHandlers -> blockchainProtocolClient
  }

  private def notificationReciprocated[F[_]: Async: Logger: FToFuture, T: Show](
    protocol:      NotificationProtocol[T],
    notifications: F[Source[T, NotUsed]],
    byteA:         Byte,
    byteB:         Byte
  )(implicit
    materializer: Materializer,
    tPushTypeTag: NetworkTypeTag[TypedProtocol.CommonMessages.Push[T]]
  ): F[ConnectionLeader => (NonEmptyChain[TypedSubHandler[F, CommonStates.None.type]], Source[T, NotUsed])] =
    (notificationServer[F, T](protocol, notifications), notificationClient[F, T](protocol)).tupled
      .map { case (f1, (f2, source)) =>
        (connectionLeader: ConnectionLeader) =>
          (ReciprocatedTypedSubHandler(f1, f2, byteA, byteB).handlers(connectionLeader), source)
      }

  private def requestResponseReciprocated[F[_]: Async: Logger: FToFuture, T](
    protocol:              RequestResponseProtocol[TypedIdentifier, T],
    fetch:                 TypedIdentifier => F[Option[T]],
    byteA:                 Byte,
    byteB:                 Byte
  )(implicit materializer: Materializer, tResponseTypeTag: NetworkTypeTag[TypedProtocol.CommonMessages.Response[T]]): F[
    ConnectionLeader => (NonEmptyChain[TypedSubHandler[F, CommonStates.None.type]], TypedIdentifier => F[Option[T]])
  ] =
    (requestResponseServer(protocol, fetch), requestResponseClient(protocol)).tupled
      .map { case (f1, (f2, callback)) =>
        (connectionLeader: ConnectionLeader) =>
          (ReciprocatedTypedSubHandler(f1, f2, byteA, byteB).handlers(connectionLeader), callback)
      }

  private def notificationServer[F[_]: Async: Logger: FToFuture, T: Show](
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
            .tapAsyncF(1)(_ => Logger[F].info(s"Remote peer requested notifications of type=${tPushTypeTag.name}"))
            .flatMapConcat(_ =>
              notifications_
                .tapAsyncF(1)(data => Logger[F].info(show"Notifying peer of data=$data"))
                .map(TypedProtocol.CommonMessages.Push(_))
                .map(OutboundMessage(_))
            ),
          codec = multiplexerCodec
        )
    )

  private def notificationClient[F[_]: Async: Logger: FToFuture, T: Show](
    protocol:              NotificationProtocol[T]
  )(implicit materializer: Materializer, tPushTypeTag: NetworkTypeTag[TypedProtocol.CommonMessages.Push[T]]) =
    for {
      (((offerF, completeF), demandSignal), source) <- Sync[F].defer(
        Source
          .backpressuredQueue[F, T](128)
          .tapAsyncF(1)(data => Logger[F].info(show"Remote peer sent notification data=$data"))
          // A Notification Client must send a `Start` message to the server before it will start
          // pushing notifications. We can signal this message to the server once the returned Source here requests
          // data for the first time
          .viaMat(OnFirstDemandFlow[T])(Keep.both)
          .preMaterialize()
          .pure[F]
      )
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
            .future(demandSignal)
            .map(_ => OutboundMessage(TypedProtocol.CommonMessages.Start))
            .concat(Source.never)
            .alsoTo(Sink.onComplete(res => implicitly[FToFuture[F]].apply(completeF(res.failed.toOption)))),
          multiplexerCodec
        )
    } yield (
      subHandler,
      source.alsoTo(Sink.onComplete(res => implicitly[FToFuture[F]].apply(completeF(res.failed.toOption))))
    )

  private def requestResponseServer[F[_]: Async: FToFuture, T](
    protocol:              RequestResponseProtocol[TypedIdentifier, T],
    fetch:                 TypedIdentifier => F[Option[T]]
  )(implicit materializer: Materializer, tResponseTypeTag: NetworkTypeTag[TypedProtocol.CommonMessages.Response[T]]) =
    for {
      ((offerResponseF, completeResponsesF), responsesSource) <- Sync[F].delay(
        Source.backpressuredQueue[F, Option[T]](32).preMaterialize()
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
          .alsoTo(Sink.onComplete(res => implicitly[FToFuture[F]].apply(completeResponsesF(res.failed.toOption)))),
        multiplexerCodec
      )

  private def requestResponseClient[F[_]: Async: FToFuture: Logger, T](
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
          () =>
            Sync[F].delay(serverSentGoPromise.success(())) >> Logger[F].info(
              s"Server is accepting request-response requests of type=${tResponseTypeTag.name}"
            )
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
        .backpressuredQueue[F, OutboundMessage](32)
        .preMaterialize()
        .pure[F]
      clientCallback = (id: TypedIdentifier) =>
        Sync[F]
          .delay(Promise[Option[T]]())
          .flatMap(promise =>
            currentResponsePromiseRef
              .set(promise.some)
              .flatTap(_ => offerOutboundMessage(OutboundMessage(TypedProtocol.CommonMessages.Get(id))))
              .productR(Async[F].fromFuture(promise.future.pure[F]))
          )

      subHandler = (sessionId: Byte) =>
        TypedSubHandler(
          sessionId,
          instance,
          TypedProtocol.CommonStates.None,
          Source
            .future(serverSentGoPromise.future)
            .flatMapConcat(_ =>
              outboundMessagesSource
                .alsoTo(
                  Sink.onComplete(res => implicitly[FToFuture[F]].apply(completeOutboundMessages(res.failed.toOption)))
                )
            ),
          multiplexerCodec
        )
    } yield (subHandler, clientCallback)

}
