package co.topl.crypto.signing

import co.topl.models.Bytes
import co.topl.models.utility.{Lengths, Sized}
import org.bouncycastle.crypto.digests.SHA512Digest
import org.bouncycastle.crypto.macs.HMac
import org.bouncycastle.crypto.params.KeyParameter

import java.nio.{ByteBuffer, ByteOrder}

package object bip32ed25519 {

  def hmac512WithKey(key: Bytes, data: Bytes): Bytes = {
    val mac = new HMac(new SHA512Digest())
    mac.init(new KeyParameter(key.toArray))
    mac.update(data.toArray, 0, data.length)
    val out = new Array[Byte](64)
    mac.doFinal(out, 0)
    Bytes(out)
  }

  sealed trait KeyIndex {

    /**
     * The size of a `Long` byte representation.
     */
    private val longByteSize = 8

    /**
     * The Index value as a `Long`.
     */
    val value: Long

    /**
     * The index representation as a 4-byte vector.
     */
    val bytes: Sized.Strict[Bytes, Lengths.`4`.type] =
      // cut off top 4 significant bytes since representation is an unsigned integer
      Sized.strictUnsafe[(
        Bytes(ByteBuffer.allocate(longByteSize).order(ByteOrder.LITTLE_ENDIAN).putLong(value).array().take(4))
      )
  }

  object KeyIndex {

    /**
     * The lower bound of the hardened index range.
     * Lower bound is `2^31` or 2147483648.
     * Upper bound is `2^32` or 4294967296.
     */
    val hardenedIndex: Long = 2147483648L

    /**
     * Instantiates a new soft-index value.
     * Any value below 0 will return a 0-value `SoftIndex`.
     * @param value the index value
     * @return a soft index
     */
    def soft(value: Int): KeyIndexes.Soft =
      if (value >= 0) KeyIndexes.Soft(value.toLong)
      else KeyIndexes.Soft(0)

    /**
     * Instantiates a new hardened-index value.
     * Any value below 0 will return a 0-value `HardenedIndex`.
     * @param value the index value
     * @return a hardened index
     */
    def hardened(value: Int): KeyIndexes.Hardened =
      if (value >= 0) KeyIndexes.Hardened(value.toLong + hardenedIndex)
      else KeyIndexes.Hardened(0.toLong + hardenedIndex)

  }

  object KeyIndexes {
    case class Soft private (override val value: Long) extends KeyIndex
    case class Hardened private (override val value: Long) extends KeyIndex

  }

}
