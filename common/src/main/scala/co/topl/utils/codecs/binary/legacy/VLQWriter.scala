package co.topl.utils.codecs.binary.legacy

import co.topl.utils.Int128
import co.topl.utils.codecs.binary.ZigZagEncoder._
import co.topl.utils.codecs.binary.valuetypes.stringCharacterSet

import java.util

trait VLQWriter extends Writer {

  def toBytes: Array[Byte]

  /**
   * Encode signed Short using ZigZag and then VLQ.
   * Both negative and positive values are supported, but due to ZigZag encoding positive
   * values is done less efficiently than by [[putUShort]].
   * Use [[putUShort]] to encode values that are always positive.
   *
   * @see [[https://en.wikipedia.org/wiki/Variable-length_quantity]]
   * @note Have to be decoded '''only''' with [[VLQReader.getShort]]
   *       The resulting varint uses ZigZag encoding, which is much more efficient at
   *       encoding negative values than pure VLQ.
   * @param x signed Short
   */
  @inline override def putShort(x: Short): this.type =
    putULong(encodeZigZagInt(x))

  /**
   * Encode unsigned Short value using VLQ.
   * Only positive values are supported, Use [[putShort]]
   * to encode negative and positive values.
   *
   * @see [[https://en.wikipedia.org/wiki/Variable-length_quantity]]
   * @param x unsigned Short in a range 0 <= x <= 0xFFFF represented as Int
   * throws `AssertionError` for values not in unsigned Short range
   */
  @inline override def putUShort(x: Int): this.type = {
    require(x >= 0 && x <= 0xffff, s"Value $x is out of unsigned short range")
    putUInt(x)
  }

  /**
   * Encode signed Int using VLQ with ZigZag.
   * Both negative and positive values are supported, but due to ZigZag encoding positive
   * values is done less efficiently than by [[putUInt]].
   * Use [[putUInt]] to encode values that are positive.
   *
   * @see [[https://en.wikipedia.org/wiki/Variable-length_quantity]]
   * @note Have to be decoded '''only''' with [[VLQReader.getInt]]
   *       The resulting varint uses ZigZag encoding, which is much more efficient at
   *       encoding negative values than pure VLQ.
   * @param x signed Int
   */
  @inline override def putInt(x: Int): this.type = putULong(encodeZigZagInt(x))

  /**
   * Encode unsigned Int value using VLQ.
   * Only positive values are supported. Use [[putInt]]
   * to encode negative and positive values.
   *
   * @see [[https://en.wikipedia.org/wiki/Variable-length_quantity]]
   * @param x unsigned Int
   * throws `AssertionError` for values not in unsigned Int range
   */
  @inline override def putUInt(x: Long): this.type = {
    require(x >= 0 && x <= 0xffffffffL, s"$x is out of unsigned int range")
    putULong(x)
  }

  /**
   * Encode signed Long using VLQ with ZigZag.
   * Both negative and positive values are supported, but due to ZigZag encoding positive
   * values is done less efficiently than by [[putULong]].
   * Use [[putULong]] to encode values that are positive.
   *
   * @see [[https://en.wikipedia.org/wiki/Variable-length_quantity]]
   * @note Have to be decoded '''only''' with [[VLQReader.getLong]]
   *       The resulting varint uses ZigZag encoding, which is much more efficient at
   *       encoding negative values than pure VLQ.
   * @param x prefer signed Long
   */
  @inline override def putLong(x: Long): this.type = putULong(encodeZigZagLong(x))

  /**
   * Encode signed Long value using VLQ.
   * Both negative and positive values are supported, but only positive values are encoded
   * efficiently, negative values are taking a toll and use six bytes. Use [[putLong]]
   * to encode negative and positive values.
   *
   * @see [[https://en.wikipedia.org/wiki/Variable-length_quantity]]
   * @note Don't use it for negative values, the resulting varint is always ten
   *       bytes long – it is, effectively, treated like a very large unsigned integer.
   *       If you use [[putLong]], the resulting varint uses ZigZag encoding,
   *       which is much more efficient.
   * @param x prefer unsigned Long (signed value will produce a significant overhead,
   *          see note above)
   */
  @inline override def putULong(x: Long): this.type = {
    val buffer = new Array[Byte](10)
    var position = 0
    var value = x
    // should be fast if java -> scala conversion did not botched it
    // source: http://github.com/google/protobuf/blob/a7252bf42df8f0841cf3a0c85fdbf1a5172adecb/java/core/src/main/java/com/google/protobuf/CodedOutputStream.java#L1387
    while (true)
      if ((value & ~0x7fL) == 0) {
        buffer(position) = value.asInstanceOf[Byte]
        position += 1
        putBytes(util.Arrays.copyOf(buffer, position))
        return this
      } else {
        buffer(position) = ((value.asInstanceOf[Int] & 0x7f) | 0x80).toByte
        position += 1
        value >>>= 7
      }
    this
    // see https://rosettacode.org/wiki/Variable-length_quantity for implementations in other languages
  }

  /** Insert the custom Int128 type that is 16 bytes (128 bits) */
  @inline def putInt128(x: Int128): this.type = this.putBytes(x.toByteArray)

  @inline override def putBits(xs: Array[Boolean]): this.type = {
    if (xs.isEmpty) return this
    val bitSet = new util.BitSet(xs.length)
    xs.zipWithIndex.foreach { case (bool, i) => bitSet.set(i, bool) }
    // pad the byte array to fix the "no bit was set" behaviour
    // see https://stackoverflow.com/questions/11209600/how-do-i-convert-a-bitset-initialized-with-false-in-a-byte-containing-0-in-java
    val bytes = util.Arrays.copyOf(bitSet.toByteArray, (xs.length + 7) / 8)
    putBytes(bytes)
    this
  }

  @inline override def putOption[T](x: Option[T])(putValue: (this.type, T) => Unit): this.type = {
    x match {
      case Some(v) =>
        put(1.toByte)
        putValue(this, v)
      case None =>
        put(0.toByte)
    }
    this
  }

  /**
   * Encode a `String` that is shorter than 256 bytes.
   *
   * @param s String the value to encode
   */
  override def putByteString(s: String): this.type = {
    val bytes = s.getBytes(stringCharacterSet)
    require(bytes.length < 256)
    put(bytes.length.toByte)
    putBytes(bytes)
    this
  }

  /**
   * Encode a `String` that is smaller than 2147483647 bytes.
   *
   * @param s String the value to encode
   */
  override def putIntString(s: String): this.type = {
    val bytes = s.getBytes(stringCharacterSet)
    require(bytes.length < Int.MaxValue)
    putUInt(bytes.length)
    putBytes(bytes)
    this
  }
}
