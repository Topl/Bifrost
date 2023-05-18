package co.topl.networking

import cats.data.{EitherT, NonEmptyChain, OptionT}
import cats.effect.kernel.Sync
import cats.effect.std.{Queue, Semaphore}
import cats.effect.{Async, Deferred, Resource}
import cats.implicits._
import cats.{Applicative, MonadThrow, Show}
import co.topl.catsutils._
import co.topl.codecs.bytes.typeclasses.Transmittable
import co.topl.networking.blockchain.NetworkTypeTags._
import co.topl.networking.multiplexer.MultiplexerCodecs._
import co.topl.networking.multiplexer._
import co.topl.networking.p2p._
import co.topl.networking.typedprotocols.TypedProtocol.CommonStates
import co.topl.networking.typedprotocols._
import fs2._
import org.typelevel.log4cats.Logger

import java.util.concurrent.TimeoutException
import scala.concurrent.duration._

/**
 * Assists with lifting a raw Socket into a TypedProtocol setting.
 * @tparam Client a Client type for interacting with the live-running connection from the application
 */
trait TypedProtocolSetFactory[F[_], Client] {

  /**
   * For some peer and leader, produces a set of sub handlers and a client
   */
  def protocolsForPeer(
    connectedPeer:    ConnectedPeer,
    connectionLeader: ConnectionLeader
  ): F[(NonEmptyChain[TypedSubHandler[F, _]], Client)]

  /**
   * Runs the typed protocols and client on top of the given socket
   * @param useClient a function which consumes the Client instance
   * @param connectedPeer The remote peer
   * @param connectionLeader The leader of the socket
   * @param reads A stream of incoming bytes
   * @param writes A stream of outgoing bytes
   * @return A resource which runs the typed protocols and client
   */
  def multiplexed(useClient: Client => Resource[F, Unit])(
    connectedPeer:    ConnectedPeer,
    connectionLeader: ConnectionLeader,
    reads:            Stream[F, Byte],
    writes:           Pipe[F, Byte, Nothing]
  )(implicit L: Logger[F], A: Async[F]): Resource[F, Unit] =
    multiplexerHandlersIn(connectedPeer, connectionLeader)
      .flatMap { case (subHandlers, client) =>
        (Multiplexer(subHandlers)(reads, writes), useClient(client)).parTupled.void
      }

  private def multiplexerHandlersIn(
    connectedPeer:    ConnectedPeer,
    connectionLeader: ConnectionLeader
  )(implicit L: Logger[F], A: Async[F]): Resource[F, (NonEmptyChain[SubHandler[F]], Client)] =
    Resource
      .eval(protocolsForPeer(connectedPeer, connectionLeader))
      .flatMap { case (typedProtocolSet, client) =>
        typedProtocolSet
          .traverse { multiplexedSubHandler =>
            val sh =
              multiplexedSubHandler.asInstanceOf[TypedSubHandler[F, multiplexedSubHandler.InState]]
            val s = sh.initialState.asInstanceOf[Any]
            implicit val ct: NetworkTypeTag[Any] = sh.initialStateNetworkTypeTag.asInstanceOf[NetworkTypeTag[Any]]
            for {
              // Stores a deferred callback result.  The Typed Protocol Instance processes messages in the background.
              // If background processing completes (successfully or with an error), the Deferred is completed.
              // This acts as a signal to the underlying data stream to complete successfully or error the stream
              instanceCompletion <- Resource.eval(Deferred[F, Either[Throwable, Unit]])
              instanceCompletionStream = Stream.exec(instanceCompletion.get.rethrow)
              applier <- sh.instance.applier(s)(instanceCompletion.complete(_).void)
              producer = handlerSource(multiplexedSubHandler, applier, multiplexedSubHandler.sessionId)
                .mergeHaltR(instanceCompletionStream)
              subHandler = SubHandler[F](
                multiplexedSubHandler.sessionId,
                s => s.through(handlerSink(multiplexedSubHandler, applier, multiplexedSubHandler.sessionId)),
                producer
              )
            } yield subHandler
          }
          .tupleRight(client)
      }

  private def handlerSink(
    multiplexedSubHandler: TypedSubHandler[F, _],
    applier:               TypedProtocolInstance[F]#MessageApplier,
    protocolInstanceId:    Byte
  )(implicit L: Logger[F], A: Async[F]): Pipe[F, Chunk[Byte], Unit] =
    _.evalMap(MessageParserFramer.parseWhole[F])
      .map { case (prefix, data) =>
        val protoByteString = com.google.protobuf.ByteString.copyFrom(data.toByteBuffer)
        multiplexedSubHandler.codec
          .decode(prefix)(protoByteString)
          .leftMap(failure => new IllegalArgumentException(failure.toString))
      }
      .rethrow
      .evalTap(_ => Logger[F].debug(s"Received inbound message in protocolInstanceId=$protocolInstanceId"))
      .evalTap { case (decodedData, networkTypeTag) =>
        applier.apply(decodedData, multiplexedSubHandler.instance.localParty.opposite)(
          networkTypeTag.asInstanceOf[NetworkTypeTag[Any]]
        )
      }
      .void

  private def handlerSource(
    multiplexedSubHandler: TypedSubHandler[F, _],
    applier:               TypedProtocolInstance[F]#MessageApplier,
    protocolInstanceId:    Byte
  )(implicit A: Async[F], L: Logger[F]): Stream[F, Chunk[Byte]] =
    multiplexedSubHandler.outboundMessages
      .evalTap(outboundMessage =>
        applier
          .apply(outboundMessage.data, multiplexedSubHandler.instance.localParty)(
            outboundMessage.networkTypeTag.asInstanceOf[NetworkTypeTag[Any]]
          )
      )
      .evalTap(o => Logger[F].debug(s"Sending outbound message in protocolInstanceId=$protocolInstanceId. ${o.data}"))
      .map(o =>
        multiplexedSubHandler.codec
          .encode(o.data)(o.networkTypeTag.asInstanceOf[NetworkTypeTag[Any]])
          .leftMap(failure => new IllegalArgumentException(failure.toString))
          .map { case (prefix, protobufByteString) =>
            (prefix, Chunk.byteBuffer(protobufByteString.asReadOnlyByteBuffer()))
          }
      )
      .rethrow
      .through(MessageSerializerFramer())
}

object TypedProtocolSetFactory {

  object CommonProtocols {

    def notificationReciprocated[F[_]: Async: Logger, T: Show: Transmittable](
      protocol:      NotificationProtocol[T],
      notifications: Stream[F, T],
      byteA:         Byte,
      byteB:         Byte
    )(implicit
      tPushTypeTag: NetworkTypeTag[TypedProtocol.CommonMessages.Push[T]]
    ): F[ConnectionLeader => (NonEmptyChain[TypedSubHandler[F, CommonStates.None.type]], Stream[F, T])] =
      (notificationServer[F, T](protocol, notifications), notificationClient[F, T](protocol)).tupled
        .map { case (f1, (f2, source)) =>
          (connectionLeader: ConnectionLeader) =>
            (ReciprocatedTypedSubHandler(f1, f2, byteA, byteB).handlers(connectionLeader), source)
        }

    def requestResponseReciprocated[F[_]: Async: Logger, Query: Transmittable, T: Transmittable](
      protocol: RequestResponseProtocol[Query, T],
      fetch:    Query => F[Option[T]],
      byteA:    Byte,
      byteB:    Byte
    )(implicit
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

    def notificationServer[F[_]: Async: Logger, T: Show: Transmittable](
      protocol:      NotificationProtocol[T],
      notifications: Stream[F, T]
    )(implicit
      tPushTypeTag: NetworkTypeTag[TypedProtocol.CommonMessages.Push[T]]
    ): F[Byte => TypedSubHandler[F, CommonStates.None.type]] =
      for {
        clientSignalPromise <- Deferred[F, Unit]
        protocolInstance <- Sync[F].delay {
          val transitions =
            new protocol.StateTransitionsServer[F](() => clientSignalPromise.complete(()).void)
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
      } yield (
        (sessionId: Byte) =>
          TypedSubHandler(
            sessionId,
            instance = protocolInstance,
            initialState = TypedProtocol.CommonStates.None,
            outboundMessages = Stream.eval(clientSignalPromise.get) >>
              notifications
                .dropOldest(64)
                .evalTap(data => Logger[F].debug(show"Notifying peer of data=$data"))
                .map(TypedProtocol.CommonMessages.Push(_))
                .map(OutboundMessage(_)),
            codec = multiplexerCodec
          )
      )

    def notificationClient[F[_]: Async: Logger, T: Show: Transmittable](
      protocol: NotificationProtocol[T]
    )(implicit
      tPushTypeTag: NetworkTypeTag[TypedProtocol.CommonMessages.Push[T]]
    ): F[(Byte => TypedSubHandler[F, CommonStates.None.type], Stream[F, T])] =
      for {
        startSignal <- Deferred[F, OutboundMessage]
        queue       <- Queue.circularBuffer[F, T](64)
        stream =
          // A Notification Client must send a `Start` message to the server before it will start
          // pushing notifications. We can signal this message to the server once the returned Source here requests
          // data for the first time
          Stream.eval(startSignal.complete(OutboundMessage(TypedProtocol.CommonMessages.Start))) >>
          Stream
            .fromQueueUnterminated(queue)
            .evalTap(data => Logger[F].debug(show"Remote peer sent notification data=$data"))
        instance <- Sync[F].delay {
          val transitions =
            new protocol.StateTransitionsClient[F](queue.offer)
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
            Stream.eval(startSignal.get) ++ Stream.never[F],
            multiplexerCodec
          )
      } yield (subHandler, stream)

    def requestResponseServer[F[_]: Async, Query: Transmittable, T: Transmittable](
      protocol: RequestResponseProtocol[Query, T],
      fetch:    Query => F[Option[T]]
    )(implicit
      queryGetTypeTag:  NetworkTypeTag[TypedProtocol.CommonMessages.Get[Query]],
      tResponseTypeTag: NetworkTypeTag[TypedProtocol.CommonMessages.Response[T]]
    ): F[Byte => TypedSubHandler[F, CommonStates.None.type]] =
      for {
        queue <- Queue.bounded[F, Option[T]](1)
        stream = Stream.fromQueueUnterminated(queue)
        transitions = new protocol.ServerStateTransitions[F](
          fetch(_)
            .flatMap(queue.tryOffer)
            .flatMap(wasAccepted =>
              Applicative[F].whenA(!wasAccepted)(
                MonadThrow[F]
                  .raiseError(
                    new IllegalStateException("Remote peer has not accepted previous RequestResponse offer yet")
                  )
              )
            )
        )
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
          Stream(OutboundMessage(TypedProtocol.CommonMessages.Start)) ++
          stream.map(TypedProtocol.CommonMessages.Response(_)).map(OutboundMessage(_)),
          multiplexerCodec
        )

    def requestResponseClient[F[_]: Async: Logger, Query: Transmittable, T: Transmittable](
      protocol: RequestResponseProtocol[Query, T]
    )(implicit
      queryGetTypeTag:  NetworkTypeTag[TypedProtocol.CommonMessages.Get[Query]],
      tResponseTypeTag: NetworkTypeTag[TypedProtocol.CommonMessages.Response[T]]
    ): F[(Byte => TypedSubHandler[F, CommonStates.None.type], Query => F[Option[T]])] =
      for {
        responsePromisesQueue  <- Sync[F].defer(Queue.bounded[F, Deferred[F, Option[T]]](1))
        serverSentStartPromise <- Deferred[F, Unit]
        requestPermit          <- Semaphore[F](1).map(_.permit)
        transitions =
          new protocol.ClientStateTransitions[F](
            r =>
              OptionT(responsePromisesQueue.tryTake).foldF(
                MonadThrow[F].raiseError(new IllegalStateException("Unexpected response from server")).void
              )(_.complete(r).void),
            () =>
              serverSentStartPromise.complete(()) >>
              Logger[F].debug(s"Server is accepting request-response requests of type=${tResponseTypeTag.name}")
          )
        instance = {
          import transitions._
          TypedProtocolInstance(Parties.B)
            .withTransition(startNoneIdle)
            .withTransition(getIdleBusy)
            .withTransition(responseBusyIdle)
            .withTransition(doneIdleDone)
        }
        queue <- Queue.bounded[F, OutboundMessage](1)
        stream = Stream.fromQueueUnterminated(queue)
        clientCallback = (query: Query) =>
          requestPermit.use(_ =>
            for {
              deferred <- Deferred[F, Option[T]]
              _        <- responsePromisesQueue.offer(deferred)
              _        <- queue.offer(OutboundMessage(TypedProtocol.CommonMessages.Get(query)))
              result <- EitherT(
                Async[F].race(
                  Async[F].delayBy(
                    Async[F].delay(new TimeoutException(s"RequestResponse failed for query=$query")),
                    5.seconds
                  ),
                  deferred.get
                )
              ).rethrowT
            } yield result
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
            Stream.eval(serverSentStartPromise.get) >> stream,
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
  outboundMessages: Stream[F, OutboundMessage],
  codec:            MultiplexerCodec
) {
  type InState = InitialState
  def initialStateNetworkTypeTag: NetworkTypeTag[InitialState] = implicitly
}

case class TypedProtocolTransitionFailureException(message: Any, reason: TypedProtocolTransitionFailure)
    extends Exception(s"TypedProtocolTransitionFailureException(message=$message, reason=$reason)") {

  override def toString: String =
    s"TypedProtocolTransitionFailureException(message=$message, reason=$reason)"

  override def getMessage: String = toString
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
case class ReciprocatedTypedSubHandler[F[_], InitialState](
  serverHandlerF: Byte => TypedSubHandler[F, InitialState],
  clientHandlerF: Byte => TypedSubHandler[F, InitialState],
  byteA:          Byte,
  byteB:          Byte
) {

  def handlers(connectionLeader: ConnectionLeader): NonEmptyChain[TypedSubHandler[F, InitialState]] =
    NonEmptyChain(serverHandler(connectionLeader), clientHandler(connectionLeader))

  def serverHandler(connectionLeader: ConnectionLeader): TypedSubHandler[F, InitialState] =
    serverHandlerF(if (connectionLeader == ConnectionLeader.Local) byteA else byteB)

  def clientHandler(connectionLeader: ConnectionLeader): TypedSubHandler[F, InitialState] =
    clientHandlerF(if (connectionLeader == ConnectionLeader.Local) byteB else byteA)
}
