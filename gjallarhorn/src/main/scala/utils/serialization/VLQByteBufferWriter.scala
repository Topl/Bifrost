package utils.serialization

import utils.ByteArrayBuilder
import utils.serialization.Writer.Aux

/**
 * Not thread safe
 */
class VLQByteBufferWriter(b: ByteArrayBuilder) extends Writer with VLQWriter {
  override type CH = ByteArrayBuilder

  @inline override def newWriter(): Aux[ByteArrayBuilder] =
    new VLQByteBufferWriter(new ByteArrayBuilder())

  @inline override def putChunk(chunk: ByteArrayBuilder): this.type = {
    b.append(chunk.toBytes)
    this
  }

  @inline override def put(x: Byte): this.type = {
    b.append(x)
    this
  }

  @inline override def putBoolean(x: Boolean): this.type = {
    b.append(x)
    this
  }

  @inline override def putBytes(xs: Array[Byte]): this.type = {
    b.append(xs)
    this
  }

  @inline override def length(): Int = b.length()

  @inline override def result(): ByteArrayBuilder = b

  @inline override def toBytes: Array[Byte] =
    b.toBytes
}
