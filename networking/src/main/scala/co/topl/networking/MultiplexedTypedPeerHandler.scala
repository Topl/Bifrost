package co.topl.networking

import akka.NotUsed
import akka.stream.scaladsl.{Flow, Sink, Source}
import akka.util.ByteString
import cats.implicits._
import cats.{~>, Applicative, Monad}
import cats.implicits._
import co.topl.networking.multiplexer.{MessageParserFramer, MessageSerializerFramer, Multiplexer, SubHandler}
import co.topl.networking.p2p.ConnectedPeer
import co.topl.networking.typedprotocols.TypedProtocolInstance
import scodec.bits.ByteVector

import scala.concurrent.Future
import scala.reflect.ClassTag

trait MultiplexedTypedPeerHandler[F[_]] {

  def protocolsForPeer(connectedPeer: ConnectedPeer): F[Source[List[MultiplexedTypedSubHandler[F]], NotUsed]]

  def multiplexed(
    connectedPeer:   ConnectedPeer
  )(implicit monadF: Monad[F], fToFuture: F ~> Future): F[Flow[ByteString, ByteString, NotUsed]] =
    multiplexerHandlersIn(connectedPeer).map(Multiplexer(_))

  private def multiplexerHandlersIn(
    connectedPeer:         ConnectedPeer
  )(implicit applicativeF: Applicative[F], fToFuture: F ~> Future): F[Source[List[SubHandler], NotUsed]] =
    protocolsForPeer(connectedPeer)
      .map(
        _.map(typedProtocolSet =>
          typedProtocolSet.toList.map(multiplexedSubHandler =>
            SubHandler(
              multiplexedSubHandler.id,
              handlerSink(multiplexedSubHandler, multiplexedSubHandler.id),
              handlerSource(multiplexedSubHandler, multiplexedSubHandler.id)
            )
          )
        )
          .concat(Source.never)
      )

  private def handlerSink(
    multiplexedSubHandler: MultiplexedTypedSubHandler[F],
    protocolInstanceId:    Byte
  )(implicit fToFuture:    F ~> Future): Sink[ByteString, NotUsed] =
    MessageParserFramer()
      .map { case (prefix, data) =>
        val (decodedData, classTag) = multiplexedSubHandler.codec.decode(prefix)(ByteVector(data.toArray))
        println(s"protocolInstanceId=$protocolInstanceId Applying remote produced message $decodedData")
        multiplexedSubHandler.applier.apply(decodedData, multiplexedSubHandler.localParty.opposite)(
          classTag.asInstanceOf[ClassTag[Any]]
        )
      }
      .mapAsync(1)(fToFuture.apply)
      .map {
        case Right(value) =>
          println(s"protocolInstanceId=$protocolInstanceId transitioned to state $value")
        case Left(error) =>
          throw new IllegalStateException(error.toString)
      }
      .to(Sink.ignore)

  private def handlerSource(
    multiplexedSubHandler: MultiplexedTypedSubHandler[F],
    protocolInstanceId:    Byte
  )(implicit fToFuture:    F ~> Future, applicativeF: Applicative[F]): Source[ByteString, _] =
    multiplexedSubHandler.outboundMessages
      .mapAsync(1) { o =>
        println(s"protocolInstanceId=$protocolInstanceId Applying locally produced message ${o.data}")
        fToFuture.apply(
          multiplexedSubHandler.applier
            .apply(o.data, multiplexedSubHandler.localParty.opposite)(
              o.classTag.asInstanceOf[ClassTag[Any]]
            )
            .map {
              case Right(value) =>
                println(s"protocolInstanceId=$protocolInstanceId transitioned to state $value")
                o
              case Left(error) =>
                throw new IllegalStateException(error.toString)
            }
        )
      }
      .map { o =>
        val (prefix, byteVector) =
          multiplexedSubHandler.codec.encode(o.data)(o.classTag.asInstanceOf[ClassTag[Any]])
        prefix -> ByteString(byteVector.toArray)
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

  def withDecoder[T: ClassTag](prefix: Byte)(f: ByteVector => T): MultiplexerCodecBuilder =
    copy(decoders = decoders.updated(prefix, data => f(data) -> implicitly[ClassTag[T]]))

  def withEncoder[T: ClassTag](prefix: Byte)(f: T => ByteVector): MultiplexerCodecBuilder =
    copy(encoders = encoders.updated(implicitly[ClassTag[T]], data => prefix -> f(data.asInstanceOf[T])))

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

case class MultiplexedTypedSubHandler[F[_]](
  id:               Byte,
  localParty:       Party,
  applier:          TypedProtocolInstance[F]#MessageApplier,
  outboundMessages: Source[OutboundMessage, _],
  codec:            MultiplexerCodec
)
