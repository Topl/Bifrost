package co.topl.networking

import akka.NotUsed
import akka.stream.Materializer
import akka.stream.scaladsl.{Flow, Keep, Sink, Source}
import akka.util.ByteString
import cats.Show
import cats.data.{EitherT, NonEmptyChain}
import cats.effect.Async
import cats.effect.kernel.Sync
import cats.effect.std.Queue
import cats.implicits._
import co.topl.catsakka._
import co.topl.codecs.bytes.typeclasses.Transmittable
import co.topl.networking.blockchain.NetworkTypeTags._
import co.topl.networking.multiplexer.MultiplexerCodecs._
import co.topl.networking.multiplexer._
import co.topl.networking.p2p.{ConnectedPeer, ConnectionLeader, ConnectionLeaders}
import co.topl.networking.typedprotocols.TypedProtocol.CommonStates
import co.topl.networking.typedprotocols._
import org.typelevel.log4cats.Logger
import scodec.bits.ByteVector

import scala.concurrent.Promise

/**
 * Helper for transforming a collection of Typed Sub Handlers into a multiplexed akka stream Flow
 * @tparam F
 * @tparam Client a Client type for interacting with the live-running connection.  (This value is materialized by the
 *                multiplexer Flow)
 */
trait TypedProtocolSetFactory[F[_], Client] {

  /**
   * For some peer and leader, produces a set of sub handlers and a client
   */
  def protocolsForPeer(
    connectedPeer:    ConnectedPeer,
    connectionLeader: ConnectionLeader
  ): F[(NonEmptyChain[TypedSubHandler[F, _]], Client)]
}

object TypedProtocolSetFactory {

  trait Ops {

    implicit class TypedProtocolSetFactoryMultiplexer[F[_]: Async: FToFuture, Client](
      factory: TypedProtocolSetFactory[F, Client]
    ) {

      def multiplexed(
        connectedPeer:    ConnectedPeer,
        connectionLeader: ConnectionLeader
      ): F[Flow[ByteString, ByteString, Client]] =
        multiplexerHandlersIn(connectedPeer, connectionLeader).map { case (subHandlers, client) =>
          Multiplexer(subHandlers, client)
        }

      private def multiplexerHandlersIn(
        connectedPeer:    ConnectedPeer,
        connectionLeader: ConnectionLeader
      ): F[(NonEmptyChain[SubHandler], Client)] =
        factory
          .protocolsForPeer(connectedPeer, connectionLeader)
          .flatMap { case (typedProtocolSet, client) =>
            typedProtocolSet
              .traverse { multiplexedSubHandler =>
                val sh =
                  multiplexedSubHandler.asInstanceOf[TypedSubHandler[F, multiplexedSubHandler.InState]]
                val s = sh.initialState.asInstanceOf[Any]
                implicit val ct: NetworkTypeTag[Any] = sh.initialStateNetworkTypeTag.asInstanceOf[NetworkTypeTag[Any]]
                sh.instance
                  .applier(s)
                  .map(applier =>
                    SubHandler(
                      multiplexedSubHandler.sessionId,
                      handlerSink(multiplexedSubHandler, applier, multiplexedSubHandler.sessionId),
                      handlerSource(multiplexedSubHandler, applier, multiplexedSubHandler.sessionId)
                    )
                  )
              }
              .tupleRight(client)
          }

      private def handlerSink(
        multiplexedSubHandler: TypedSubHandler[F, _],
        applier:               TypedProtocolInstance[F]#MessageApplier,
        protocolInstanceId:    Byte
      ): Sink[ByteString, NotUsed] =
        MessageParserFramer()
          .map { case (prefix, data) =>
            multiplexedSubHandler.codec.decode(prefix)(ByteVector(data.toArray)) match {
              case Right(value)  => value
              case Left(failure) => throw new IllegalArgumentException(failure.toString)
            }
          }
          .log(s"Received inbound message in protocolInstanceId=$protocolInstanceId", _._1)
          .mapAsyncF(1) { case (decodedData, networkTypeTag) =>
            EitherT(
              applier.apply(decodedData, multiplexedSubHandler.instance.localParty.opposite)(
                networkTypeTag.asInstanceOf[NetworkTypeTag[Any]]
              )
            )
              .leftMap(error => TypedProtocolTransitionFailureException(decodedData, error))
              .rethrowT
          }
          .to(Sink.ignore)

      private def handlerSource(
        multiplexedSubHandler: TypedSubHandler[F, _],
        applier:               TypedProtocolInstance[F]#MessageApplier,
        protocolInstanceId:    Byte
      ): Source[ByteString, _] =
        multiplexedSubHandler.outboundMessages
          .mapAsyncF(1)(outboundMessage =>
            EitherT(
              applier
                .apply(outboundMessage.data, multiplexedSubHandler.instance.localParty)(
                  outboundMessage.networkTypeTag.asInstanceOf[NetworkTypeTag[Any]]
                )
            )
              .leftMap(error => TypedProtocolTransitionFailureException(outboundMessage.data, error))
              .rethrowT
              .as(outboundMessage)
          )
          .log(s"Sending outbound message in protocolInstanceId=$protocolInstanceId", o => o.data)
          .map { o =>
            val (prefix, byteVector) =
              multiplexedSubHandler.codec.encode(o.data)(o.networkTypeTag.asInstanceOf[NetworkTypeTag[Any]]) match {
                case Right(value)  => value
                case Left(failure) => throw new IllegalArgumentException(failure.toString)
              }
            prefix -> ByteString(byteVector.toArray)
          }
          .via(MessageSerializerFramer())

    }
  }

  object implicits extends Ops

  object CommonProtocols {

    def notificationReciprocated[F[_]: Async: Logger: FToFuture, T: Show: Transmittable](
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

    def requestResponseReciprocated[F[_]: Async: Logger: FToFuture, Query: Transmittable, T: Transmittable](
      protocol: RequestResponseProtocol[Query, T],
      fetch:    Query => F[Option[T]],
      byteA:    Byte,
      byteB:    Byte
    )(implicit
      materializer:     Materializer,
      queryGetTypeTag:  NetworkTypeTag[TypedProtocol.CommonMessages.Get[Query]],
      tResponseTypeTag: NetworkTypeTag[TypedProtocol.CommonMessages.Response[T]]
    ): F[
      ConnectionLeader => (NonEmptyChain[TypedSubHandler[F, CommonStates.None.type]], Query => F[Option[T]])
    ] =
      (requestResponseServer(protocol, fetch), requestResponseClient(protocol)).tupled
        .map { case (f1, (f2, callback)) =>
          (connectionLeader: ConnectionLeader) =>
            (ReciprocatedTypedSubHandler(f1, f2, byteA, byteB).handlers(connectionLeader), callback)
        }

    def notificationServer[F[_]: Async: Logger: FToFuture, T: Show: Transmittable](
      protocol:      NotificationProtocol[T],
      notifications: F[Source[T, NotUsed]]
    )(implicit
      tPushTypeTag: NetworkTypeTag[TypedProtocol.CommonMessages.Push[T]]
    ): F[Byte => TypedSubHandler[F, CommonStates.None.type]] =
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
        multiplexerCodec = MultiplexerCodecBuilder()
          .withCodec[TypedProtocol.CommonMessages.Start.type](1: Byte)
          .withCodec[TypedProtocol.CommonMessages.Done.type](2: Byte)
          .withCodec[TypedProtocol.CommonMessages.Push[T]](3: Byte)
          .multiplexerCodec
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

    def notificationClient[F[_]: Async: Logger: FToFuture, T: Show: Transmittable](
      protocol: NotificationProtocol[T]
    )(implicit
      materializer: Materializer,
      tPushTypeTag: NetworkTypeTag[TypedProtocol.CommonMessages.Push[T]]
    ): F[(Byte => TypedSubHandler[F, CommonStates.None.type], Source[T, NotUsed])] =
      for {
        (((offerF, completeF), demandSignal), source) <- Sync[F].defer(
          Source
            .backpressuredQueue[F, T](16)
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
        multiplexerCodec = MultiplexerCodecBuilder()
          .withCodec[TypedProtocol.CommonMessages.Start.type](1: Byte)
          .withCodec[TypedProtocol.CommonMessages.Done.type](2: Byte)
          .withCodec[TypedProtocol.CommonMessages.Push[T]](3: Byte)
          .multiplexerCodec
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

    def requestResponseServer[F[_]: Async: FToFuture, Query: Transmittable, T: Transmittable](
      protocol: RequestResponseProtocol[Query, T],
      fetch:    Query => F[Option[T]]
    )(implicit
      materializer:     Materializer,
      queryGetTypeTag:  NetworkTypeTag[TypedProtocol.CommonMessages.Get[Query]],
      tResponseTypeTag: NetworkTypeTag[TypedProtocol.CommonMessages.Response[T]]
    ): F[Byte => TypedSubHandler[F, CommonStates.None.type]] =
      for {
        ((offerResponseF, completeResponsesF), responsesSource) <- Sync[F].delay(
          Source.backpressuredQueue[F, Option[T]](4).preMaterialize()
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
        multiplexerCodec = MultiplexerCodecBuilder()
          .withCodec[TypedProtocol.CommonMessages.Start.type](1: Byte)
          .withCodec[TypedProtocol.CommonMessages.Done.type](2: Byte)
          .withCodec[TypedProtocol.CommonMessages.Get[Query]](3: Byte)
          .withCodec[TypedProtocol.CommonMessages.Response[T]](4: Byte)
          .multiplexerCodec
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

    def requestResponseClient[F[_]: Async: FToFuture: Logger, Query: Transmittable, T: Transmittable](
      protocol: RequestResponseProtocol[Query, T]
    )(implicit
      materializer:     Materializer,
      queryGetTypeTag:  NetworkTypeTag[TypedProtocol.CommonMessages.Get[Query]],
      tResponseTypeTag: NetworkTypeTag[TypedProtocol.CommonMessages.Response[T]]
    ): F[(Byte => TypedSubHandler[F, CommonStates.None.type], Query => F[Option[T]])] =
      for {
        responsePromisesQueue <- Sync[F].defer(Queue.bounded[F, Promise[Option[T]]](1))
        serverSentStartPromise = Promise[Unit]()
        transitions =
          new protocol.ClientStateTransitions[F](
            r => responsePromisesQueue.take.map(_.success(r)),
            () =>
              Sync[F].delay(serverSentStartPromise.success(())) >> Logger[F].info(
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
          .backpressuredQueue[F, OutboundMessage](4)
          .preMaterialize()
          .pure[F]
        clientCallback = (query: Query) =>
          Sync[F]
            .delay(Promise[Option[T]]())
            .flatMap(promise =>
              responsePromisesQueue
                .offer(promise)
                .flatTap(_ => offerOutboundMessage(OutboundMessage(TypedProtocol.CommonMessages.Get(query))))
                .productR(Async[F].fromFuture(promise.future.pure[F]))
            )
        multiplexerCodec = MultiplexerCodecBuilder()
          .withCodec[TypedProtocol.CommonMessages.Start.type](1: Byte)
          .withCodec[TypedProtocol.CommonMessages.Done.type](2: Byte)
          .withCodec[TypedProtocol.CommonMessages.Get[Query]](3: Byte)
          .withCodec[TypedProtocol.CommonMessages.Response[T]](4: Byte)
          .multiplexerCodec
        subHandler = (sessionId: Byte) =>
          TypedSubHandler(
            sessionId,
            instance,
            TypedProtocol.CommonStates.None,
            Source
              .future(serverSentStartPromise.future)
              .flatMapConcat(_ =>
                outboundMessagesSource
                  .alsoTo(
                    Sink
                      .onComplete(res => implicitly[FToFuture[F]].apply(completeOutboundMessages(res.failed.toOption)))
                  )
              ),
            multiplexerCodec
          )
      } yield (subHandler, clientCallback)
  }

}

/**
 * Captures the information needed to send the given data to a peer through a multiplexer
 */
class OutboundMessage private (val data: Any, val networkTypeTag: NetworkTypeTag[_])

object OutboundMessage {
  def apply[T: NetworkTypeTag](t: T): OutboundMessage = new OutboundMessage(t, implicitly[NetworkTypeTag[T]])
}

/**
 * Wraps a TypedProtocolInstance (which properly handles inbound messages) with supplemental information needed to
 * run through a multiplexed peer connection
 * @param sessionId the unique identifier of this typed protocol instance _within a multiplexer instance_
 * @param instance the instance of the Typed Protocol
 * @param initialState the state to pass into the TypedProtocolInstance's applier factory
 * @param outboundMessages A Source of messages to send to the remote peer
 * @param codec the Multiplexer codec which can serialize/deserialize network messages
 */
case class TypedSubHandler[F[_], InitialState: NetworkTypeTag](
  sessionId:        Byte,
  instance:         TypedProtocolInstance[F],
  initialState:     InitialState,
  outboundMessages: Source[OutboundMessage, _],
  codec:            MultiplexerCodec
) {
  type InState = InitialState
  def initialStateNetworkTypeTag: NetworkTypeTag[InitialState] = implicitly
}

case class TypedProtocolTransitionFailureException(message: Any, reason: TypedProtocolTransitionFailure)
    extends Exception {

  override def toString: String =
    s"TypedProtocolTransitionFailureException(message=$message, reason=$reason)"
}

/**
 * Often times, a Typed Protocol is meant to act in a (client, server) pair, meaning a client instance and a server instance
 * of the protocol run within the same multiplexer.
 * @param serverHandlerF
 * @param clientHandlerF
 * @param byteA The session ID byte of the server when the local node is the connection leader, or the ID byte of the
 *              client when the remote node is the connection leader
 * @param byteB The session ID byte of the client when the local node is the connection leader, or the ID byte of the
 *              server when the remote node is the connection leader
 */
case class ReciprocatedTypedSubHandler[F[_], InitialState: NetworkTypeTag](
  serverHandlerF: Byte => TypedSubHandler[F, InitialState],
  clientHandlerF: Byte => TypedSubHandler[F, InitialState],
  byteA:          Byte,
  byteB:          Byte
) {

  def handlers(
    connectionLeader: ConnectionLeader
  ): NonEmptyChain[TypedSubHandler[F, InitialState]] =
    NonEmptyChain(serverHandler(connectionLeader), clientHandler(connectionLeader))

  def serverHandler(connectionLeader: ConnectionLeader): TypedSubHandler[F, InitialState] = serverHandlerF(
    if (connectionLeader == ConnectionLeaders.Local) byteA else byteB
  )

  def clientHandler(connectionLeader: ConnectionLeader): TypedSubHandler[F, InitialState] = clientHandlerF(
    if (connectionLeader == ConnectionLeaders.Local) byteB else byteA
  )
}
