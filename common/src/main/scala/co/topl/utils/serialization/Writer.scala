package co.topl.utils.serialization

import co.topl.utils.Int128

trait Writer {

  /**
   * Type of encoded data
   */
  type CH

  /**
   * Length of encoded data
   * @return
   */
  def length(): Int

  /**
   * Creates new instance of this Writer
   * @return new instance of Writer
   */
  def newWriter(): Writer.Aux[CH]

  /**
   * Append result of `writer` to this Writer
   * @param writer is used as source of bytes
   * @return
   */
  def append(writer: Writer.Aux[CH]): this.type =
    putChunk(writer.result())

  /**
   * Encode chunk
   * @param chunk to put into this Writer
   * @return
   */
  def putChunk(chunk: CH): this.type

  /**
   * Encode signed byte
   * @param x byte value to encode
   * @return
   */
  def put(x: Byte): this.type

  /**
   * Encode integer as an unsigned byte asserting the range check
   * @param x integer value to encode
   * @return
   * throws `AssertionError` if x is outside of the unsigned byte range
   */
  def putUByte(x: Int): this.type = {
    require(x >= 0 && x <= 0xff, s"$x is out of unsigned byte range")
    put(x.toByte)
  }

  /**
   * Encode boolean
   * @param x boolean value to encode
   * @return
   */
  def putBoolean(x: Boolean): this.type

  /**
   * Encode signed Short
   *
   * Use [[putUShort]] to encode values that are positive.
   * @param x short value to encode
   * @return
   */
  def putShort(x: Short): this.type

  /**
   * Encode Short that are positive
   *
   * Use [[putShort]] to encode values that might be negative.
   * @param x Short
   */
  def putUShort(x: Int): this.type

  /**
   * Encode signed Int.
   * Use [[putUInt]] to encode values that are positive.
   *
   * @param x Int
   */
  def putInt(x: Int): this.type

  /**
   * Encode Int that are positive.
   * Use [[putInt]] to encode values that might be negative.
   *
   * @param x Int
   */
  def putUInt(x: Long): this.type

  /**
   * Encode signed Long.
   * Use [[putULong]] to encode values that are positive.
   *
   * @param x Long
   */
  def putLong(x: Long): this.type

  /**
   * Encode Long that are positive.
   * Use [[putLong]] to encode values that might be negative.
   *
   * @param x Long
   */
  def putULong(x: Long): this.type

  /**
   * Encode an Int128 value
   * @param x - Int128
   */
  def putInt128(x: Int128): this.type

  /**
   * Encode an array of bytes
   * @param xs value to encode
   * @return
   */
  def putBytes(xs: Array[Byte]): this.type

  /**
   * Encode an array of boolean values as a bit array
   *
   * @param xs array of boolean values
   */
  def putBits(xs: Array[Boolean]): this.type

  /**
   * Encode optional value
   * @param x optional value to encode
   * @param putValue procedure to encode value, if `x` is nonempty
   */
  def putOption[T](x: Option[T])(putValue: (this.type, T) => Unit): this.type

  /**
   * Encode String is shorter than 256 bytes
   * @param s String
   * @return
   */
  def putByteString(s: String): this.type

  /**
   * Encode String is shorter than 256 bytes
   * @param s String
   * @return
   */
  def putIntString(s: String): this.type

  /**
   * Returns encoded result
   * @return encoded result
   */
  def result(): CH

}

object Writer {
  type Aux[CCH] = Writer { type CH = CCH }
}
