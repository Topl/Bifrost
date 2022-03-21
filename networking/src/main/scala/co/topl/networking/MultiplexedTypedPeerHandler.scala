package co.topl.networking

import akka.NotUsed
import akka.stream.scaladsl.{Flow, Sink, Source}
import akka.util.ByteString
import cats.data.EitherT
import cats.effect.Sync
import cats.implicits._
import cats.{~>, Monad, MonadThrow}
import co.topl.codecs.bytes.typeclasses.Transmittable
import co.topl.networking.multiplexer.{MessageParserFramer, MessageSerializerFramer, Multiplexer, SubHandler}
import co.topl.networking.p2p.{ConnectedPeer, ConnectionLeader}
import co.topl.networking.typedprotocols.TypedProtocolInstance
import scodec.bits.ByteVector

import scala.concurrent.Future
import scala.reflect.ClassTag
import scala.util.Try

trait MultiplexedTypedPeerHandler[F[_]] {

  def protocolsForPeer(
    connectedPeer:    ConnectedPeer,
    connectionLeader: ConnectionLeader
  ): F[Source[List[MultiplexedTypedSubHandler[F, _]], NotUsed]]

  def multiplexed(
    connectedPeer:    ConnectedPeer,
    connectionLeader: ConnectionLeader
  )(implicit monadF:  Monad[F], syncF: Sync[F], fToFuture: F ~> Future): F[Flow[ByteString, ByteString, NotUsed]] =
    multiplexerHandlersIn(connectedPeer, connectionLeader).map(Multiplexer(_))

  private def multiplexerHandlersIn(
    connectedPeer:    ConnectedPeer,
    connectionLeader: ConnectionLeader
  )(implicit monadF:  Monad[F], syncF: Sync[F], fToFuture: F ~> Future): F[Source[List[SubHandler], NotUsed]] =
    protocolsForPeer(connectedPeer, connectionLeader)
      .map(
        _.mapAsync(1)(typedProtocolSet =>
          implicitly[F ~> Future].apply(
            typedProtocolSet.traverse { multiplexedSubHandler =>
              val sh = multiplexedSubHandler.asInstanceOf[MultiplexedTypedSubHandler[F, multiplexedSubHandler.InState]]
              val s = sh.initialState.asInstanceOf[Any]
              implicit val ct: ClassTag[Any] = sh.initialStateClassTag.asInstanceOf[ClassTag[Any]]
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
          )
        )
          .concat(Source.never)
      )

  private def handlerSink(
    multiplexedSubHandler: MultiplexedTypedSubHandler[F, _],
    applier:               TypedProtocolInstance[F]#MessageApplier,
    protocolInstanceId:    Byte
  )(implicit fToFuture:    F ~> Future, monadThrowF: MonadThrow[F]): Sink[ByteString, NotUsed] =
    MessageParserFramer()
      .mapAsync(1) { case (prefix, data) =>
        fToFuture.apply(
          EitherT(
            MonadThrow[F]
              .fromTry(Try(multiplexedSubHandler.codec.decode(prefix)(ByteVector(data.toArray))))
              .flatMap { case (decodedData, classTag) =>
                applier.apply(decodedData, multiplexedSubHandler.instance.localParty.opposite)(
                  classTag.asInstanceOf[ClassTag[Any]]
                )
              }
          ).void.value
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
                o.classTag.asInstanceOf[ClassTag[Any]]
              )
          )
            .leftMap(error => new IllegalStateException(error.toString))
            .rethrowT >>
          MonadThrow[F]
            .fromTry(Try(multiplexedSubHandler.codec.encode(o.data)(o.classTag.asInstanceOf[ClassTag[Any]])))
            .map { case (prefix, byteVector) => prefix -> ByteString(byteVector.toArray) }
        )
      }
      .via(MessageSerializerFramer())

}

trait MultiplexerCodec {
  def decode(prefix:            Byte)(data: ByteVector): (Any, ClassTag[_])
  def encode[T: ClassTag](data: T): (Byte, ByteVector)
}

case class MultiplexerCodecBuilder(
  private val decoders: Map[Byte, ByteVector => (Any, ClassTag[_])],
  private val encoders: Map[ClassTag[_], Any => (Byte, ByteVector)]
) {

  def withCodec[T: ClassTag: Transmittable](prefix: Byte): MultiplexerCodecBuilder =
    copy(
      decoders = decoders.updated(
        prefix,
        data => Transmittable[T].fromTransmittableBytes(data).getOrElse(???) -> implicitly[ClassTag[T]]
      ),
      encoders = encoders.updated(
        implicitly[ClassTag[T]],
        value => (prefix, Transmittable[T].transmittableBytes(value.asInstanceOf[T]))
      )
    )

  def multiplexerCodec: MultiplexerCodec =
    new MultiplexerCodec {

      def decode(prefix: Byte)(data: ByteVector): (Any, ClassTag[_]) =
        decoders(prefix)(data)

      def encode[T: ClassTag](data: T): (Byte, ByteVector) =
        encoders(implicitly[ClassTag[T]])(data)
    }
}

class OutboundMessage private (val data: Any, val classTag: ClassTag[_])

object OutboundMessage {
  def apply[T: ClassTag](t: T): OutboundMessage = new OutboundMessage(t, implicitly[ClassTag[T]])
}

case class MultiplexedTypedSubHandler[F[_], InitialState: ClassTag](
  id:               Byte,
  instance:         TypedProtocolInstance[F],
  initialState:     InitialState,
  outboundMessages: Source[OutboundMessage, _],
  codec:            MultiplexerCodec
) {
  type InState = InitialState
  def initialStateClassTag: ClassTag[InitialState] = implicitly
}
