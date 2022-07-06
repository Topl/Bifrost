package co.topl.crypto.generation

import co.topl.crypto.generation.Bip32Indexes.{HardenedIndex, SoftIndex}
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

object Bip32Index {

  /**
   * The lower bound of the hardened index range.
   * Lower bound is `2^31` or 2147483648.
   * Upper bound is `2^32` or 4294967296.
   */
  val hardenedOffset: Long = 2147483648L

  def apply(value: Long): Bip32Index =
    if (value < hardenedOffset) SoftIndex(value)
    else HardenedIndex(value)
}

object Bip32Indexes {
  case class SoftIndex private (override val value: Long) extends Bip32Index

  object SoftIndex {
    def apply(value: Long): SoftIndex = if (value >= 0) new SoftIndex(value) else new SoftIndex(0)
  }

  case class HardenedIndex private (override val value: Long) extends Bip32Index

  object HardenedIndex {

    def apply(value: Long): HardenedIndex =
      if (value >= 0) new HardenedIndex(value + Bip32Index.hardenedOffset)
      else new HardenedIndex(0 + Bip32Index.hardenedOffset)
  }

  implicit class Bip32IndexesSupport(value: Long) {
    def soft: SoftIndex = SoftIndex(value)
    def hardened: HardenedIndex = HardenedIndex(value)
  }
}
