package co.topl.networking.multiplexer

import cats.Monad
import cats.data.OptionT
import cats.effect.Async
import cats.effect.implicits._
import cats.implicits._
import co.topl.models.Bytes
import com.google.common.primitives.Ints
import com.google.protobuf.ByteString
import fs2._
import fs2.io.net.Socket

import scala.concurrent.duration._

object MultiplexedFraming {

  def apply[F[_]: Monad](socket: Socket[F]): Stream[F, (Int, Chunk[Byte])] =
    Stream
      .repeatEval(
        OptionT(socket.read(8))
          .map(prefix =>
            (
              Ints.fromBytes(prefix(0), prefix(1), prefix(2), prefix(3)),
              Ints.fromBytes(prefix(4), prefix(5), prefix(6), prefix(7))
            )
          )
          .flatMap { case (port, length) =>
            if (length == 0) OptionT.some[F]((port, Chunk.empty[Byte]))
            else OptionT(socket.read(length)).tupleLeft(port)
          }
          .value
      )
      .unNoneTerminate

  def writer[F[_]: Async](socket: Socket[F]): (Int, Chunk[Byte]) => F[Unit] =
    (port, data) =>
      socket
        .write(
          Chunk.array(Ints.toByteArray(port)) ++
          Chunk.array(Ints.toByteArray(data.size)) ++
          data
        )
        .timeout(3.seconds)
}

case class MultiplexedReaderWriter[F[_]](
  read:  Stream[F, (Int, Bytes)],
  write: (Int, Bytes) => F[Unit]
)

object MultiplexedReaderWriter {

  def forSocket[F[_]: Async](socket: Socket[F]): MultiplexedReaderWriter[F] = {
    val writer = MultiplexedFraming.writer(socket)
    MultiplexedReaderWriter[F](
      MultiplexedFraming(socket).buffer(1).map { case (port, chunk) =>
        (port, ByteString.copyFrom(chunk.toByteBuffer))
      },
      (port, data) => writer.apply(port, Chunk.byteBuffer(data.asReadOnlyByteBuffer()))
    )
  }
}
