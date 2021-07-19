package utils.serialization

import utils.Extensions._
import utils.ZigZagEncoder._

import java.util

trait VLQReader extends Reader {

  @inline override def getUByte(): Int = getByte() & 0xff

  /**
   * Decode signed Short previously encoded with [[VLQWriter.putShort]] using VLQ and then ZigZag.
   *
   * @note Uses VLQ and then ZigZag encoding. Should be used to decode '''only''' a value that was previously
   *       encoded with [[VLQByteBufferWriter.putShort]].
   * @see [[https://en.wikipedia.org/wiki/Variable-length_quantity]]
   * @return signed Short
   */
  @inline override def getShort(): Short =
    // should only be changed simultaneously with `putInt`
    decodeZigZagInt(getULong().toInt).toShort

  /**
   * Decode Short previously encoded with [[VLQWriter.putUShort]] using VLQ.
   * @see [[https://en.wikipedia.org/wiki/Variable-length_quantity]]
   * @return Int
   * throws `AssertionError` for deserialized values not in unsigned Short range
   */
  @inline override def getUShort(): Int = {
    val x = getULong().toInt
    require(x >= 0 && x <= 0xffff, s"$x is out of unsigned short range")
    x
  }

  /**
   * Decode signed Int previously encoded with [[VLQWriter.putInt]] using VLQ with ZigZag.
   *
   * @note Uses ZigZag encoding. Should be used to decode '''only''' a value that was previously
   *       encoded with [[VLQByteBufferWriter.putInt]].
   * @see [[https://en.wikipedia.org/wiki/Variable-length_quantity]]
   * @return signed Int
   */
  @inline override def getInt(): Int =
    // should only be changed simultaneously with `putInt`
    decodeZigZagInt(getULong().toInt)

  /**
   * Decode Int previously encoded with [[VLQWriter.putUInt]] using VLQ.
   * @see [[https://en.wikipedia.org/wiki/Variable-length_quantity]]
   * @return Long
   */
  @inline override def getUInt(): Long = {
    val x = getULong()
    require(x >= 0L && x <= 0xffffffffL, s"$x is out of unsigned int range")
    x
  }

  /**
   * Decode signed Long previously encoded with [[VLQWriter.putLong]] using VLQ with ZigZag.
   *
   * @note Uses ZigZag encoding. Should be used to decode '''only''' a value that was previously
   *       encoded with [[VLQWriter.putLong]].
   * @see [[https://en.wikipedia.org/wiki/Variable-length_quantity]]
   * @return signed Long
   */
  @inline override def getLong(): Long = decodeZigZagLong(getULong())

  /**
   * Decode Long previously encoded with [[VLQWriter.putULong]] using VLQ.
   * @see [[https://en.wikipedia.org/wiki/Variable-length_quantity]]
   * @return Long
   */
  @inline override def getULong(): Long = {
    // should be fast if java -> scala conversion did not botched it
    // source: http://github.com/google/protobuf/blob/a7252bf42df8f0841cf3a0c85fdbf1a5172adecb/java/core/src/main/java/com/google/protobuf/CodedInputStream.java#L2653
    // for faster version see: http://github.com/google/protobuf/blob/a7252bf42df8f0841cf3a0c85fdbf1a5172adecb/java/core/src/main/java/com/google/protobuf/CodedInputStream.java#L1085
    var result: Long = 0
    var shift = 0
    while (shift < 64) {
      val b = getByte()
      result = result | ((b & 0x7f).toLong << shift)
      if ((b & 0x80) == 0) return result
      shift += 7
    }
    sys.error(s"Cannot deserialize Long value. Unexpected reader $this with bytes remaining $remaining")
    // see https://rosettacode.org/wiki/Variable-length_quantity for implementations in other languages
  }

  @inline override def getBits(size: Int): Array[Boolean] = {
    if (size == 0) return Array[Boolean]()
    val bitSet = util.BitSet.valueOf(getBytes((size + 7) / 8))
    val boolArray = new Array[Boolean](size)
    var i = 0
    while (i < size) {
      boolArray(i) = bitSet.get(i)
      i += 1
    }
    boolArray
  }

  @inline override def getOption[T](getValue: => T): Option[T] = {
    val tag = getByte()
    if (tag != 0)
      Some(getValue)
    else
      None
  }

  /**
   * Decode String is shorter than 256 bytes
   *
   * @return
   */
  @inline override def getByteString(): String = {
    val size = getUByte()
    new String(getBytes(size))
  }

  /**
   * Decode String is greater than 256 bytes
   *
   * @return
   */
  @inline override def getIntString(): String = {
    val size = getUInt().toIntExact
    new String(getBytes(size))
  }
}
