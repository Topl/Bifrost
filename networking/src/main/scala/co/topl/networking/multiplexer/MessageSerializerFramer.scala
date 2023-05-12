package co.topl.networking.multiplexer

import fs2.Chunk
import fs2._

import java.nio.ByteBuffer

/**
 * An Akka Flow which serializes "typed data" (meaning, data bytes which with a byte prefix indicating the data's type).
 *
 * The data is formatted as: prefix + data length + data
 */
object MessageSerializerFramer {

  def apply[F[_]](): Pipe[F, (Byte, Chunk[Byte]), Chunk[Byte]] =
    _.map((function _).tupled)

  def function(typeByte: Byte, chunk: Chunk[Byte]): Chunk[Byte] =
    Chunk.singleton(typeByte) ++
    Chunk.array(ByteBuffer.allocate(4).putInt(chunk.size).array()) ++
    chunk
}
