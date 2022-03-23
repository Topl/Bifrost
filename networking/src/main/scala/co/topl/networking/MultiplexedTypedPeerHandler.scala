package co.topl.networking

import akka.NotUsed
import akka.event.Logging
import akka.stream.Attributes
import akka.stream.scaladsl.{Flow, Sink, Source}
import akka.util.ByteString
import cats.data.EitherT
import cats.effect.GenConcurrent
import cats.implicits._
import cats.{~>, MonadThrow}
import co.topl.codecs.bytes.typeclasses.Transmittable
import co.topl.networking.multiplexer.{MessageParserFramer, MessageSerializerFramer, Multiplexer, SubHandler}
import co.topl.networking.p2p.{ConnectedPeer, ConnectionLeader}
import co.topl.networking.typedprotocols.{TypedProtocolInstance, TypedProtocolTransitionFailure}
import scodec.bits.ByteVector

import scala.concurrent.Future

trait MultiplexedTypedPeerHandler[F[_], Client] {

  def protocolsForPeer(
    connectedPeer:    ConnectedPeer,
    connectionLeader: ConnectionLeader
  ): F[(List[MultiplexedTypedSubHandler[F, _]], Client)]

  def multiplexed(
    connectedPeer:    ConnectedPeer,
    connectionLeader: ConnectionLeader
  )(implicit
    genConcurrentF: GenConcurrent[F, Throwable],
    fToFuture:      F ~> Future
  ): F[Flow[ByteString, ByteString, Client]] =
    multiplexerHandlersIn(connectedPeer, connectionLeader).map { case (handlers, client) =>
      Multiplexer(handlers, client)
    }

  private def multiplexerHandlersIn(
    connectedPeer:    ConnectedPeer,
    connectionLeader: ConnectionLeader
  )(implicit
    genConcurrentF: GenConcurrent[F, Throwable],
    fToFuture:      F ~> Future
  ): F[(List[SubHandler], Client)] =
    protocolsForPeer(connectedPeer, connectionLeader)
      .flatMap { case (typedProtocolSet, client) =>
        typedProtocolSet
          .traverse { multiplexedSubHandler =>
            val sh =
              multiplexedSubHandler.asInstanceOf[MultiplexedTypedSubHandler[F, multiplexedSubHandler.InState]]
            val s = sh.initialState.asInstanceOf[Any]
            implicit val ct: NetworkTypeTag[Any] = sh.initialStateNetworkTypeTag.asInstanceOf[NetworkTypeTag[Any]]
            sh.instance
              .applier(s)
              .map(applier =>
                SubHandler(
                  multiplexedSubHandler.id,
                  handlerSink(multiplexedSubHandler, applier, multiplexedSubHandler.id),
                  handlerSource(multiplexedSubHandler, applier, multiplexedSubHandler.id)
                )
              )
          }
          .tupleRight(client)
      }

  private def handlerSink(
    multiplexedSubHandler: MultiplexedTypedSubHandler[F, _],
    applier:               TypedProtocolInstance[F]#MessageApplier,
    protocolInstanceId:    Byte
  )(implicit fToFuture:    F ~> Future, monadThrowF: MonadThrow[F]): Sink[ByteString, NotUsed] =
    MessageParserFramer()
      .map { case (prefix, data) => multiplexedSubHandler.codec.decode(prefix)(ByteVector(data.toArray)) }
      .log(s"Received inbound message in protocolInstanceId=$protocolInstanceId", _._1)
      .mapAsync(1) { case (decodedData, networkTypeTag) =>
        fToFuture.apply(
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
    multiplexedSubHandler: MultiplexedTypedSubHandler[F, _],
    applier:               TypedProtocolInstance[F]#MessageApplier,
    protocolInstanceId:    Byte
  )(implicit fToFuture:    F ~> Future, monadThrowF: MonadThrow[F]): Source[ByteString, _] =
    multiplexedSubHandler.outboundMessages
      .mapAsync(1) { o =>
        fToFuture.apply(
          EitherT(
            applier
              .apply(o.data, multiplexedSubHandler.instance.localParty)(
                o.networkTypeTag.asInstanceOf[NetworkTypeTag[Any]]
              )
          )
            .leftMap(error => TypedProtocolTransitionFailureException(o.data, error))
            .rethrowT
            .as(o)
        )
      }
      .log(s"Sending outbound message in protocolInstanceId=$protocolInstanceId", o => o.data)
      .map { o =>
        val (prefix, byteVector) =
          multiplexedSubHandler.codec.encode(o.data)(o.networkTypeTag.asInstanceOf[NetworkTypeTag[Any]])
        prefix -> ByteString(byteVector.toArray)
      }
      .via(MessageSerializerFramer())

}

trait MultiplexerCodec {
  def decode(prefix:                  Byte)(data: ByteVector): (Any, NetworkTypeTag[_])
  def encode[T: NetworkTypeTag](data: T): (Byte, ByteVector)
}

case class MultiplexerCodecBuilder(
  private val decoders: Map[Byte, ByteVector => (Any, NetworkTypeTag[_])],
  private val encoders: Map[NetworkTypeTag[_], Any => (Byte, ByteVector)]
) {

  def withCodec[T: NetworkTypeTag: Transmittable](prefix: Byte): MultiplexerCodecBuilder =
    copy(
      decoders = decoders.updated(
        prefix,
        data => Transmittable[T].fromTransmittableBytes(data).getOrElse(???) -> implicitly[NetworkTypeTag[T]]
      ),
      encoders = encoders.updated(
        implicitly[NetworkTypeTag[T]],
        value => (prefix, Transmittable[T].transmittableBytes(value.asInstanceOf[T]))
      )
    )

  def multiplexerCodec: MultiplexerCodec =
    new MultiplexerCodec {

      def decode(prefix: Byte)(data: ByteVector): (Any, NetworkTypeTag[_]) =
        decoders(prefix)(data)

      def encode[T: NetworkTypeTag](data: T): (Byte, ByteVector) =
        encoders(implicitly[NetworkTypeTag[T]])(data)
    }
}

class OutboundMessage private (val data: Any, val networkTypeTag: NetworkTypeTag[_])

object OutboundMessage {
  def apply[T: NetworkTypeTag](t: T): OutboundMessage = new OutboundMessage(t, implicitly[NetworkTypeTag[T]])
}

case class MultiplexedTypedSubHandler[F[_], InitialState: NetworkTypeTag](
  id:               Byte,
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
