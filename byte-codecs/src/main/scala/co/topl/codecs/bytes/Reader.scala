package co.topl.codecs.bytes

import co.topl.models.Int128

trait Reader {

  /**
   * Type of encoded data
   */
  type CH

  /**
   * Creates new instance of this Reader
   * @param chunk encoded data
   * @return
   */
  def newReader(chunk: CH): Reader.Aux[CH]

  /**
   * Returns encoded data at current position
   * @param size
   * @return
   */
  def getChunk(size: Int): CH

  /**
   * Decode String that is shorter than 256 in size
   * @return
   */
  def getByteString(): String

  /**
   * Decode String longer than 256 bytes
   * @return
   */
  def getIntString(): String

  /**
   * Get a byte at current position without advancing the position.
   * @return byte at current position
   */
  def peekByte(): Byte

  /**
   * Decode a boolean
   * @return boolean
   */
  def getBoolean(): Boolean

  /**
   * Decode signed byte
   * @return Byte
   */
  def getByte(): Byte

  /**
   * Decode positive Byte
   * @return signed Int
   */
  def getUByte(): Int

  /**
   * Decode signed short
   * @return Short
   */
  def getShort(): Short

  /**
   * Decode positive Short.
   * @return signed Int
   */
  def getUShort(): Int

  /**
   * Decode signed Int.
   * @return signed Int
   */
  def getInt(): Int

  /**
   * Decode positive Int.
   * @return signed Long
   */
  def getUInt(): Long

  /**
   * Decode signed Long.
   * @return signed Long
   */
  def getLong(): Long

  /**
   * Decode positive Long.
   * @return signed Long
   */
  def getULong(): Long

  /**
   * Decode sign Int128
   * @return signed Int128
   */
  def getInt128(): Int128

  /**
   * Decode array of byte values
   * @param size expected size of decoded array
   * @return
   */
  def getBytes(size: Int): Array[Byte]

  /**
   * Decode array of boolean values
   * @param size expected size of decoded array
   * @return decoded array of boolean values
   */
  def getBits(size: Int): Array[Boolean]

  /**
   * Decode optional value
   * @param getValue function to decode value, if optional value is nonempty
   * @return optional value
   */
  def getOption[T](getValue: => T): Option[T]

  /**
   * Sets the mark to current position
   */
  def mark(): this.type

  /**
   * Returns the number of decoded elements
   * @return The number of decoded elements
   */
  def consumed: Int

  /**
   * Returns current position
   * @return position
   */
  def position: Int

  /**
   * Sets position
   * @param p position
   */
  def position_=(p: Int): Unit

  /**
   * Returns the number of elements between the current position and the
   * end of Reader
   * @return The number of elements remaining in th Reader
   */
  def remaining: Int
}

object Reader {
  type Aux[CCH] = Reader { type CH = CCH }
}
