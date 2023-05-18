package co.topl.networking.multiplexer

import cats.MonadThrow
import cats.implicits._
import fs2._

/**
 * An FS2 Pipe which deserializes "typed data" (meaning, data bytes which with a byte prefix indicating the data's type).
 *
 * The expected data is formatted is: prefix + data length + data
 */
object MessageParserFramer {

  def apply[F[_]: RaiseThrowable](): Pipe[F, Byte, (Byte, Chunk[Byte])] = {

    /**
     * Read the first byte from the stream.  This byte indicates a "type prefix".  Once read, call `readSize`.
     */
    def readTypePrefix(s: Stream[F, Byte]): Pull[F, (Byte, Chunk[Byte]), Unit] =
      s.pull.uncons1.flatMap {
        case Some((typeByte, tail)) =>
          readSize(tail, typeByte)
        case None =>
          Pull.done
      }

    /**
     * Read the next 4 bytes from the stream.  These 4 bytes encode an integer in Big-Endian format.
     * Once read, calls `readData`.
     */
    def readSize(s: Stream[F, Byte], typeByte: Byte) =
      s.pull.unconsN(4).flatMap {
        case Some((sizeBytes, tail)) =>
          val size = sizeBytes.toByteBuffer.getInt
          readData(tail, typeByte, size)
        case None =>
          Pull.raiseError[F](new IllegalStateException("Unexpected end-of-stream"))
      }

    /**
     * Reads `size` number of bytes from the stream, representing the _actual_ data of the message.
     * Once read, the stream outputs a (type, data) tuple, and the process restarts from `readTypePrefix` for
     * the remainder of the stream.
     */
    def readData(s: Stream[F, Byte], typeByte: Byte, size: Int) =
      s.pull.unconsN(size).flatMap {
        case Some((chunk, tail)) =>
          Pull.output(Chunk.singleton((typeByte, chunk))) >> readTypePrefix(tail)
        case None =>
          Pull.raiseError[F](new IllegalStateException("Unexpected end-of-stream"))
      }

    readTypePrefix(_).stream
  }

  /**
   * Parses a "complete" framed chunk of byte data.
   * @param bytes Input data represented as (type prefix) :+ (size bytes len 4) ++ (data)
   *              The input should be at least 5 bytes (1 for the prefix, 4 for the size).
   * @return A tuple (type prefix, data)
   */
  def parseWhole[F[_]: MonadThrow](bytes: Chunk[Byte]): F[(Byte, Chunk[Byte])] =
    for {
      _ <- MonadThrow[F].raiseWhen(bytes.size < 5)(new IllegalArgumentException("Invalid byte chunk header"))
      typeByte = bytes(0)
      // Decode the size bytes (length 4) in Big-Endian format
      sizeBytes = bytes.drop(1).take(4)
      size = sizeBytes.toByteBuffer.getInt
      _ <- MonadThrow[F].raiseWhen(bytes.size != (5 + size))(new IllegalArgumentException("Invalid byte chunk data"))
      data = bytes.drop(5)
    } yield (typeByte, data)
}
