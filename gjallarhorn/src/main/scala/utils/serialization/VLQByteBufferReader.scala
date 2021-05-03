package utils.serialization

import java.nio.ByteBuffer

import utils.serialization.Reader.Aux

/**
 * Not thread safe
 */
class VLQByteBufferReader(buf: ByteBuffer) extends VLQReader {

  type CH = ByteBuffer

  @inline override def newReader(chunk: ByteBuffer): Aux[ByteBuffer] =
    new VLQByteBufferReader(chunk)

  @inline override def getChunk(size: Int): ByteBuffer = ByteBuffer.wrap(getBytes(size))

  @inline override def peekByte(): Byte = buf.array()(buf.position())

  @inline
  override def getBoolean(): Boolean =
    if (getByte() == 0x01) true
    else false

  @inline override def getByte(): Byte = buf.get

  @inline override def getBytes(size: Int): Array[Byte] = {
    val res = new Array[Byte](size)
    buf.get(res)
    res
  }

  private var _mark: Int = _

  @inline override def mark(): this.type = {
    _mark = buf.position()
    this
  }
  @inline override def consumed: Int = buf.position() - _mark

  @inline override def position: Int = buf.position()

  @inline override def position_=(p: Int): Unit = buf.position(p)

  @inline override def remaining: Int = buf.remaining()
}
