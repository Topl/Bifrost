package co.topl.models.utility

import co.topl.crypto.KeyIndexes
import co.topl.models.Bytes
import co.topl.models.utility.HasLength.instances._

import java.nio.{ByteBuffer, ByteOrder}

//sealed trait Bip32Index {
//
//  /**
//   * The size of a `Long` byte representation.
//   */
//  private val longByteSize = 8
//
//  /**
//   * The Index value as a `Long`.
//   */
//  val value: Long
//
//  /**
//   * The index representation as a 4-byte vector.
//   */
//  val bytes: Sized.Strict[Bytes, Lengths.`4`.type] =
//    // cut off top 4 significant bytes since representation is an unsigned integer
//    Sized.strictUnsafe(
//      Bytes(ByteBuffer.allocate(longByteSize).order(ByteOrder.LITTLE_ENDIAN).putLong(value).array().take(4))
//    )
//}

object Bip32Index {

  /**
   * The lower bound of the hardened index range.
   * Lower bound is `2^31` or 2147483648.
   * Upper bound is `2^32` or 4294967296.
   */
  val hardenedOffset: Long = 2147483648L

  /**
   * Instantiates a new soft-index value.
   * Any value below 0 will return a 0-value `SoftIndex`.
   * @param value the index value
   * @return a soft index
   */
  def soft(value: Int): KeyIndexes.Bip32.Soft =
    if (value >= 0) KeyIndexes.Bip32.Soft(value.toLong)
    else KeyIndexes.Bip32.Soft(0)

  /**
   * Instantiates a new hardened-index value.
   * Any value below 0 will return a 0-value `HardenedIndex`.
   * @param value the index value
   * @return a hardened index
   */
  def hardened(value: Int): KeyIndexes.Bip32.Hardened =
    if (value >= 0) KeyIndexes.Bip32.Hardened(value.toLong + hardenedOffset)
    else KeyIndexes.Bip32.Hardened(0.toLong + hardenedOffset)

}
