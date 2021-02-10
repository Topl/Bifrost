package co.topl.utils

import java.math.BigInteger
import java.nio.{ByteBuffer, ByteOrder}
import java.util.UUID

import scala.language.implicitConversions
import scala.math.{BigInt, ScalaNumber, ScalaNumericConversions}
import scala.util.{Failure, Success, Try}

/* Copyright (c) 2015-2019 Stanford University
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR(S) DISCLAIM ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL AUTHORS BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

/** Container for a 128 bit signed integer stored in big-endian format.
 * Adopted from the original work available at
 * https://github.com/PlatformLab/TorcDB/blob/master/src/main/java/net/ellitron/torc/util/UInt128.java
 * by Jonathan Ellithorpe (jde@cs.stanford.edu)
  *
  * @author James Aman (j.aman@topl.me)
  */
object Int128 {
  val size = 128
  val bytes: Int = size / java.lang.Byte.SIZE
  val MinValue: Int128 = Int128(Long.MinValue, 0L)
  val MaxValue: Int128 = Int128(Long.MaxValue, -1L)

  /** This method attempts to translate its argument into a UInt128.
    *
    * @param n Number to translate.
    * @return UInt128
    */
  def decode(n: Any): Try[Int128] = Try {
    n match {
      case b: Byte          => Int128(b)
      case sh: Short        => Int128(sh)
      case integer: Integer => Int128(integer)
      case l: Long          => Int128(l)
      case idStr: String =>
        if (idStr.startsWith("0x")) Int128(idStr, 16)
        else Int128(idStr, 10)
      case integer: BigInteger => Int128(integer)
      case uuid: UUID          => Int128(uuid)
      case bytes: Array[Byte]  => Int128(bytes)
      case int12: Int128      => int12
      case _ =>
        throw new NumberFormatException(String.format("Unable to decode " + "number: [%s,%s]", n.getClass, n.toString))
    }
  }

  /** Constructs a UInt128 from two longs, one representing the upper 64 bits
    * and the other representing the lower 64 bits.
    *
    * @param upperLong Upper 64 bits.
    * @param lowerLong Lower 64 bits.
    */
  def apply(upperLong: Long, lowerLong: Long): Int128 = new Int128(upperLong, lowerLong)

  /** Constructs a UInt128 from a Byte value. Value is treated as unsigned and
    * therefore no sign extension is performed.
    *
    * @param value Byte, treated as 8 unsigned bits.
    */
  def apply(value: Byte): Int128 = new Int128(0, value.toLong)

  /** Constructs a UInt128 from a Short value. Value is treated as unsigned and
    * therefore no sign extension is performed.
    *
    * @param value Short, treated as 16 unsigned bits.
    */
  def apply(value: Short): Int128 = new Int128(0, value.toLong)

  /** Constructs a UInt128 from an Integer value. Value is treated as unsigned
    * and therefore no sign extension is performed.
    *
    * @param value Integer, treated as 32 unsigned bits.
    */
  def apply(value: Integer): Int128 = new Int128(0, value.toLong)

  /** Constructs a UInt128 from a Long value. Value is treated as unsigned and
    * therefore no sign extension is performed.
    *
    * @param value Long, treated as 64 unsigned bits.
    */
  def apply(value: Long): Int128 = new Int128(0, value)

  /** Constructs a UInt128 from a BigInteger value. The value used to construct
    * the UInt128 is the 128-bit two's complement representation of the
    * BigInteger. In the case that the number of bits in the minimal
    * two's-complement representation of the BigInteger is greater than 128,
    * then the lower 128 bits are used and higher order bits are discarded.
    *
    * @param value BigInteger, treated as a 128 bit signed value.
    */
  def apply(value: BigInt): Int128 = {
    val bigIntBytes = value.toByteArray
    val res = if (bigIntBytes.length >= Int128.bytes) {
      bigIntBytes.takeRight(16)
    } else {
      val pad: Byte = if (bigIntBytes.head < 0) -1 else 0
      Array.fill(16 - bigIntBytes.length)(pad) ++ bigIntBytes
    }

    val buf = ByteBuffer.allocate(bytes).order(ByteOrder.BIG_ENDIAN)
    buf.put(res)
    buf.flip()
    new Int128(buf.getLong(), buf.getLong())
  }

  /** Constructs a UInt128 from a String value representing a number in a
    * specified base. The value stored in this UInt128 is the 128 bit
    * two's-complement representation of this value. If the two's-complement
    * representation of the value requires more than 128 bits to represent, only
    * the lower 128 bits are used to construct this UInt128.
    *
    * @param value   String, treated as a 128 bit signed value.
    * @param radix The base of the number represented by the string.
    */
  def apply(value: String, radix: Int): Int128 = apply(new BigInteger(value, radix))

  /** Constructs a UInt128 from a String value representing a number in base 10.
    * The value stored in this UInt128 is the 128 bit two's-complement
    * representation of this value. If the two's-complement representation of
    * the value requires more than 128 bits to represent, only the lower 128
    * bits are used to construct this UInt128.
    *
    * @param value Base 10 format String, treated as a 128 bit signed value.
    */
  def apply(value: String): Int128 = apply(value, 10)

  /** Constructs a UInt128 from a UUID value.
    * The value stored in this UInt128 is the 128 bit two's-complement
    * representation of this value. If the two's-complement representation of
    * the value requires more than 128 bits to represent, only the lower 128
    * bits are used to construct this UInt128.
    *
    * @param value 128-bit UUID value
    */
  def apply(value: UUID): Int128 = new Int128(value.getMostSignificantBits, value.getLeastSignificantBits)

  /** Constructs a UInt128 from a byte array value. The byte array value is
    * interpreted as unsigned and in big-endian format. If the byte array is
    * less than 16 bytes, then the higher order bytes of the resulting UInt128
    * are padded with 0s. If the byte array is greater than 16 bytes, then the
    * lower 16 bytes are used.
    *
    * @param value Byte array in big-endian format.
    */
  def apply(value: Array[Byte]): Int128 = {
    val res = if (value.length >= Int128.bytes) {
      value.takeRight(16)
    } else {
      Array.fill(16 - value.length)(0: Byte) ++ value
    }

    val buf = ByteBuffer.allocate(bytes).order(ByteOrder.BIG_ENDIAN)
    buf.put(res)
    buf.flip()
    new Int128(buf.getLong(), buf.getLong())
  }

  /** Parses a UInt128 from a ByteBuffer. The ByteBuffer is expected to have at
    * least 16 bytes at the current position, storing a UInt128 in big-endian
    * order. This method will also advance the current position of the
    * ByteBuffer by 16 bytes.
    *
    * @param buf ByteBuffer containing the value in big-endian format at the
    *            current position of the buffer.
    */
  def parseFromByteBuffer(buf: ByteBuffer): Int128 = {
    val upperLong = buf.getLong
    val lowerLong = buf.getLong
    new Int128(upperLong, lowerLong)
  }

  /** Implicit conversion from `Int` to `BigInt`. */
  implicit def int2uint128(i: Int): Int128 = apply(i)

  /** Implicit conversion from `Long` to `BigInt`. */
  implicit def long2uint128(l: Long): Int128 = apply(l)

  /** Implicit conversion from `java.math.BigInteger` to `scala.BigInt`. */
  implicit def javaBigInteger2uint128(x: BigInteger): Int128 = apply(x)
}

final class Int128(val upperLong: Long, val lowerLong: Long)
    extends ScalaNumber
    with ScalaNumericConversions
    with Serializable
    with Ordered[Int128] {

  private val bigInt = BigInt(this.toByteArray)

  /** Returns a byte array containing this 128 bit unsigned integer in
    * big-endian format.
    *
    * @return Byte array containing this number in big-endian format.
    */
  def toByteArray: Array[Byte] = {
    val buf = ByteBuffer.allocate(Int128.bytes).order(ByteOrder.BIG_ENDIAN)
    buf.putLong(upperLong)
    buf.putLong(lowerLong)
    buf.array
  }

  /** Returns a hexadecimal String representing this 128 bit unsigned integer
    * with the minimum number of digits.
    *
    * @return Formatted string representing this number.
    */
  override def toString: String = BigInt(this.toByteArray).toString()

  def toString(radix: Int): String = bigInt.toString(radix)

  override def compare(that: Int128): Int = this.bigInt.compare(that.bigInt)

  /** If the supplied argument is another UInt128, tests for bit-wise equality,
    * otherwise attempts to convert the argument to a UInt128 and then tests for
    * equality.
    */
  override def equals(that: Any): Boolean = that match {
    case int12: Int128 => int12.upperLong == this.upperLong && int12.lowerLong == this.lowerLong
    case _ => Int128.decode(that) match {
      case Failure(_)    => false
      case Success(that) => this == that
    }
  }

  override def hashCode: Int = {
    var hash = 7
    hash = 83 * hash + (this.upperLong ^ (this.upperLong >>> 32)).toInt
    hash = 83 * hash + (this.lowerLong ^ (this.lowerLong >>> 32)).toInt
    hash
  }

  override def underlying: (Long, Long) = (this.upperLong, this.lowerLong)

  override def isWhole(): Boolean = true

  override def isValidByte: Boolean = this >= Byte.MinValue && this <= Byte.MaxValue
  override def isValidShort: Boolean = this >= Short.MinValue && this <= Short.MaxValue
  override def isValidChar: Boolean = this >= Char.MinValue && this <= Char.MaxValue
  override def isValidInt: Boolean = this >= Int.MinValue && this <= Int.MaxValue
  def isValidLong: Boolean = this >= Long.MinValue && this <= Long.MaxValue
  def isValidFloat: Boolean = BigInt(this.toByteArray).isValidFloat
  def isValidDouble: Boolean = BigInt(this.toByteArray).isValidDouble

  override def intValue(): Int = this.bigInt.intValue()

  override def longValue(): Long = this.bigInt.longValue()

  override def floatValue(): Float = this.bigInt.floatValue()

  override def doubleValue(): Double = this.bigInt.doubleValue()

  /** Subtraction of UInt128 */
  def +  (that: Int128): Int128 = Int128(this.bigInt + that.bigInt)

  /** Subtraction of UInt128 */
  def -  (that: Int128): Int128 = Int128(this.bigInt - that.bigInt)

  /** Multiplication of UInt128 */
  def *  (that: Int128): Int128 = Int128(this.bigInt * that.bigInt)

  /** Division of UInt128 */
  def /  (that: Int128): Int128 = Int128(this.bigInt / that.bigInt)

  /** Remainder of UInt128 */
  def %  (that: Int128): Int128 = Int128(this.bigInt % that.bigInt)

  /** Returns a BigInt whose value is the negation of this BigInt */
  def unary_- : Int128   = Int128(this.bigInt.bigInteger.negate())

  /** Returns the bitwise complement of this BigInt */
  def unary_~ : Int128 = Int128(this.bigInt.bigInteger.not())

}