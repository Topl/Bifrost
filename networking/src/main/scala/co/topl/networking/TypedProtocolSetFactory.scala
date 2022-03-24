package co.topl.networking

import akka.NotUsed
import akka.stream.scaladsl.{Flow, Sink, Source}
import akka.util.ByteString
import cats.data.{EitherT, NonEmptyChain}
import cats.effect.Async
import cats.implicits._
import cats.~>
import co.topl.networking.multiplexer._
import co.topl.networking.p2p.{ConnectedPeer, ConnectionLeader, ConnectionLeaders}
import co.topl.networking.typedprotocols.{TypedProtocolInstance, TypedProtocolTransitionFailure}
import scodec.bits.ByteVector

import scala.concurrent.Future

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

    implicit class TypedProtocolSetFactoryMultiplexer[F[_]: Async: *[_] ~> Future, Client](
      factory: TypedProtocolSetFactory[F, Client]
    ) {

      def multiplexed(
        connectedPeer:    ConnectedPeer,
        connectionLeader: ConnectionLeader
      ): F[Flow[ByteString, ByteString, Client]] =
        multiplexerHandlersIn(connectedPeer, connectionLeader).map { case (handlers, client) =>
          Multiplexer(handlers, client)
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
          .mapAsync(1) { case (decodedData, networkTypeTag) =>
            implicitly[F ~> Future].apply(
              EitherT(
                applier.apply(decodedData, multiplexedSubHandler.instance.localParty.opposite)(
                  networkTypeTag.asInstanceOf[NetworkTypeTag[Any]]
                )
              )
                .leftMap(error => TypedProtocolTransitionFailureException(decodedData, error))
                .rethrowT
            )
          }
          .to(Sink.ignore)

      private def handlerSource(
        multiplexedSubHandler: TypedSubHandler[F, _],
        applier:               TypedProtocolInstance[F]#MessageApplier,
        protocolInstanceId:    Byte
      ): Source[ByteString, _] =
        multiplexedSubHandler.outboundMessages
          .mapAsync(1)(outboundMessage =>
            implicitly[F ~> Future].apply(
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
