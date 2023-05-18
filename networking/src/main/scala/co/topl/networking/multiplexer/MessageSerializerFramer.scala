package co.topl.networking.multiplexer

import co.topl.networking.encodeInt
import fs2._

/**
 * A Pipe which serializes "typed data" (meaning, data bytes which with a byte prefix indicating the data's type).
 *
 * The data is formatted as: prefix + data length + data
 */
object MessageSerializerFramer {

  def apply[F[_]](): Pipe[F, (Byte, Chunk[Byte]), Chunk[Byte]] =
    _.map((function _).tupled)

  def function(typeByte: Byte, chunk: Chunk[Byte]): Chunk[Byte] =
    Chunk.singleton(typeByte) ++
    // An integer is encoded into 4 bytes (using ByteBuffer endian)
    Chunk.array(encodeInt(chunk.size)) ++
    chunk
}
