package co.topl.crypto.typeclasses

import co.topl.codecs.bytes.BasicCodecs._
import co.topl.codecs.bytes.ByteCodec.implicits._
import co.topl.crypto.signatures.Ed25519
import co.topl.crypto.typeclasses.ContainsVerificationKey.instances._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Lengths._
import co.topl.models.utility.{Lengths, Sized}
import co.topl.models.{Bytes, SecretKeys, VerificationKeys}
import org.bouncycastle.crypto.digests.SHA512Digest
import org.bouncycastle.crypto.macs.HMac
import org.bouncycastle.crypto.params.KeyParameter
import simulacrum.{op, typeclass}

import java.nio.{ByteBuffer, ByteOrder}

/**
 * Represents a value that can be further derived using some index
 */
@typeclass trait SoftDerivative[T] {

  /**
   * Derives a new value T at some index using a previous value of T
   */
  @op("softDerive") def softDerivativeOf(t: T, index: Derivative.KeyIndexes.Soft): T
}

object SoftDerivative {

  trait Instances {

    /**
     * ED-25519 Base Order N
     *
     * Equivalent to `2^252 + 27742317777372353535851937790883648493`
     */
    private val edBaseN: BigInt =
      BigInt("7237005577332262213973186563042994240857116359379907606001950938285454250989")

    implicit def extendedEd25519SKDerivative(implicit ed25519: Ed25519): SoftDerivative[SecretKeys.ExtendedEd25519] = {
      (t, index) =>
        // Note: BigInt expects Big-Endian, but SLIP/BIP-ED25519 need Little-Endian
        val leftNumber: BigInt = BigInt(1, t.leftKey.data.toArray.reverse)

        // TODO: Avoid require
        require(leftNumber % edBaseN != 0)

        val rightNumber: BigInt = BigInt(1, t.rightKey.data.toArray.reverse)

        val public =
          ContainsVerificationKey[SecretKeys.ExtendedEd25519, VerificationKeys.ExtendedEd25519].verificationKeyOf(t)

        val z =
          Derivative.hmac512WithKey(
            t.chainCode.data,
            Bytes(Array(0x02.toByte)) ++ public.bytes ++ index.bytes.data
          )

        val zLeft =
          BigInt(1, z.slice(0, 28).reverse.toArray)

        val zRight =
          BigInt(1, z.slice(32, 64).reverse.toArray)

        val nextLeft =
          Bytes(
            ByteBuffer
              .wrap(
                (zLeft * 8 + leftNumber).toByteArray.reverse
              )
              .order(ByteOrder.LITTLE_ENDIAN)
              .array()
              .take(32)
          )

        val nextRight =
          Bytes(
            ByteBuffer
              .wrap(((zRight + rightNumber) % (BigInt(2).pow(256))).toByteArray.reverse)
              .order(ByteOrder.LITTLE_ENDIAN)
              .array()
              .take(32)
          )

        val nextChainCode =
          Bytes(
            Derivative
              .hmac512WithKey(
                t.chainCode.data,
                Bytes(Array(0x03.toByte)) ++ public.bytes ++ index.bytes.data
              )
              .slice(32, 64)
              .toArray
          )

        SecretKeys.ExtendedEd25519(
          Sized.strictUnsafe(nextLeft),
          Sized.strictUnsafe(nextRight),
          Sized.strictUnsafe(nextChainCode)
        )
    }

    implicit def extendedEd25519VKDerivative(implicit ed: Ed25519): SoftDerivative[VerificationKeys.ExtendedEd25519] = {
      (t, index) =>
        val z = Derivative.hmac512WithKey(t.chainCode.data, (0x02.toByte +: t.bytes) ++ index.bytes.data)

        val zL = z.slice(0, 28)

        val zLMult8 =
          (8 * BigInt(1, zL.reverse.toArray)).toByteArray.reverse.take(32)

        val scaledZL = new ed.PointAccum
        ed.scalarMultBase(zLMult8.toArray, scaledZL)

        val publicKeyPoint = new ed.PointExt
        ed.decodePointVar(t.bytes.toArray, 0, negate = false, publicKeyPoint)

        ed.pointAddVar(negate = false, publicKeyPoint, scaledZL)

        val nextPublicKeyBytes = new Array[Byte](ed.KeyLength)
        ed.encodePoint(scaledZL, nextPublicKeyBytes, 0)

        val nextPk = Bytes(nextPublicKeyBytes)

        val nextChainCode =
          Derivative
            .hmac512WithKey(t.chainCode.data, (0x03.toByte +: t.bytes) ++ index.bytes.data)
            .slice(32, 64)

        VerificationKeys.ExtendedEd25519(
          VerificationKeys.Ed25519(Sized.strictUnsafe(nextPk)),
          Sized.strictUnsafe(nextChainCode)
        )
    }
  }
}

object Derivative {

  def hmac512WithKey(key: Bytes, data: Bytes): Bytes = {
    val mac = new HMac(new SHA512Digest())
    mac.init(new KeyParameter(key.toArray))
    mac.update(data.toArray, 0, data.length)
    val out = new Array[Byte](64)
    mac.doFinal(out, 0)
    Bytes(out)
  }

  trait KeyIndex {

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
      Sized.strictUnsafe(
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
