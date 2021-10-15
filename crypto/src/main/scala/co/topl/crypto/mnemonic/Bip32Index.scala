package co.topl.crypto.mnemonic

import co.topl.models.Bytes
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.{Lengths, Sized}

import java.nio.{ByteBuffer, ByteOrder}

sealed trait Bip32Index {

  /**
   * The Index value as a `Long`.
   */
  val value: Long

  /**
   * The index representation as a 4-byte vector.
   */
  val bytes: Sized.Strict[Bytes, Lengths.`4`.type] =
    // cut off top 4 significant bytes since representation is an unsigned integer
    Sized.strictUnsafe(
      Bytes(
        ByteBuffer
          .allocate(java.lang.Long.SIZE)
          .order(ByteOrder.LITTLE_ENDIAN)
          .putLong(value)
          .array()
          .take(4)
      )
    )
}

case class SoftIndex private (override val value: Long) extends Bip32Index
case class HardenedIndex private (override val value: Long) extends Bip32Index

object Bip32Index {
  /**
   * The lower bound of the hardened index range.
   * Lower bound is `2^31` or 2147483648.
   * Upper bound is `2^32` or 4294967296.
   */
  val hardenedOffset: Long = 2147483648L

  def apply(value: Long): Bip32Index = {
    if (value < hardenedOffset) HardenedIndex(value)
    else SoftIndex(value)
  }

  /**
   * Instantiates a new soft-index value.
   * Any value below 0 will return a 0-value `SoftIndex`.
   * @param value the index value
   * @return a soft index
   */
  def soft(value: Int): SoftIndex =
    if (value >= 0) SoftIndex(value.toLong)
    else SoftIndex(0)

  /**
   * Instantiates a new hardened-index value.
   * Any value below 0 will return a 0-value `HardenedIndex`.
   * @param value the index value
   * @return a hardened index
   */
  def hardened(value: Int): HardenedIndex =
    if (value >= 0) HardenedIndex(value.toLong + hardenedOffset)
    else HardenedIndex(0.toLong + hardenedOffset)

}
