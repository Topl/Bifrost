package co.topl.networking.multiplexer

import cats.Monad
import cats.data.OptionT
import cats.effect.{Async, Resource}
import cats.effect.implicits._
import cats.effect.std.Mutex
import cats.implicits._
import co.topl.models.Bytes
import co.topl.networking._
import com.google.common.primitives.Ints
import com.google.protobuf.ByteString
import fs2._
import fs2.io.net.Socket

import scala.concurrent.duration._

/**
 * Provides functions which read and write data to an underlying socket
 * @param read a stream of (port, data) from the remote peer
 * @param write a function which sends (port, data) to the remote peer
 */
case class MultiplexedReaderWriter[F[_]](
  read:  Stream[F, (Int, Bytes)],
  write: (Int, Bytes) => F[Unit]
)

object MultiplexedReaderWriter {

  def make[F[_]: Async](socket: Socket[F], writeTimeout: FiniteDuration): Resource[F, MultiplexedReaderWriter[F]] = {
    val _writer = writer(socket, writeTimeout)
    Mutex[F].toResource.map(mutex =>
      MultiplexedReaderWriter[F](
        reader(socket).map { case (port, chunk) =>
          (port, ByteString.copyFrom(chunk.toByteBuffer))
        },
        (port: Int, data: Bytes) => mutex.lock.surround(_writer(port, Chunk.byteBuffer(data.asReadOnlyByteBuffer())))
      )
    )
  }

  /**
   * Reads a never-ending stream of frames:
   * | port | length | data |
   *
   * `port` is a 4-byte encoded integer
   * `length` is a 4-byte encoded integer, indicating the length of `data`
   * `data` is the payload of the message
   * @param socket a Socket from which data can be read
   * @return a stream of (port, data)
   */
  def reader[F[_]: Monad](socket: Socket[F]): Stream[F, (Int, Chunk[Byte])] =
    Stream
      .repeatEval(
        OptionT(socket.readExactly(8))
          .map(prefix =>
            (
              Ints.fromBytes(prefix(0), prefix(1), prefix(2), prefix(3)),
              Ints.fromBytes(prefix(4), prefix(5), prefix(6), prefix(7))
            )
          )
          .flatMap { case (port, length) =>
            if (length == 0) OptionT.some[F]((port, Chunk.empty[Byte]))
            else OptionT(socket.readExactly(length)).tupleLeft(port)
          }
          .value
      )
      .unNoneTerminate

  /**
   * Writes a frame to the socket in the form of:
   * | port | length | data |
   *
   * `port` is a 4-byte encoded integer
   * `length` is a 4-byte encoded integer, indicating the length of `data`
   * `data` is the payload of the message
   *
   * @param socket a Socket to which data can be written
   * @return a function (port, data) => void
   */
  def writer[F[_]: Async](socket: Socket[F], writeTimeout: FiniteDuration): (Int, Chunk[Byte]) => F[Unit] =
    (port, data) =>
      socket
        .write(
          Chunk.array(Ints.toByteArray(port)) ++
          Chunk.array(Ints.toByteArray(data.size)) ++
          data
        )
        .timeout(writeTimeout)
}
