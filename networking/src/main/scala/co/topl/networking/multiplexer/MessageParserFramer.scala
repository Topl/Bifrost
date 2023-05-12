package co.topl.networking.multiplexer

import fs2._

/**
 * An FS2 Pipe which deserializes "typed data" (meaning, data bytes which with a byte prefix indicating the data's type).
 *
 * The expected data is formatted is: prefix + data length + data
 */
object MessageParserFramer {

  def apply[F[_]](): Pipe[F, Byte, (Byte, Chunk[Byte])] = {
    def start(s: Stream[F, Byte]): Pull[F, (Byte, Chunk[Byte]), Unit] =
      s.pull.unconsN(5).flatMap {
        case Some((chunk, tail)) =>
          val typeByte = chunk(0)
          val sizeBytes = chunk.drop(1)
          val size = sizeBytes.toByteBuffer.getInt
          read(tail, size, typeByte)
        case None =>
          Pull.done
      }
    def read(s: Stream[F, Byte], size: Int, typeByte: Byte) =
      s.pull.unconsN(size).flatMap {
        case Some((chunk, tail)) =>
          Pull.output(Chunk.singleton((typeByte, chunk))) >> start(tail)
        case None =>
          Pull.done
      }

    start(_).stream
  }

  def parseWhole(bytes: Chunk[Byte]): (Byte, Chunk[Byte]) = {
    val typeByte = bytes(0)
    val sizeBytes = bytes.drop(1)
    sizeBytes.toByteBuffer.getInt
    val data = bytes.drop(5)
    (typeByte, data)
  }
}
