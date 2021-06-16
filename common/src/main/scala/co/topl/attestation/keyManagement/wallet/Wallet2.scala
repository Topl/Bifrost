package co.topl.attestation.keyManagement.wallet

import co.topl.attestation.keyManagement.wallet.bip39.Mnemonic.Mnemonic
import co.topl.crypto.hash.sha512
import com.google.common.primitives.Longs
import org.bouncycastle.crypto.digests.SHA512Digest
import org.bouncycastle.crypto.macs.HMac
import org.bouncycastle.crypto.params.KeyParameter
import co.topl.crypto.signatures.eddsa.Ed25519
import co.topl.utils.codecs.{AsBytes, Infallible}

import java.nio.charset.StandardCharsets

object Wallet2 {

  sealed trait Index {
    val value: Long

    def bytes: Array[Byte] = Longs.toByteArray(value)
  }

  case class BasicIndex private (override val value: Long) extends Index
  case class HardenedIndex private (override val value: Long) extends Index

  object Index {

    private val hardenedIndex: Long = 0x80000000L

    def basic(value: Long): Option[BasicIndex] =
      if (value >= hardenedIndex) None
      else Some(BasicIndex(value))

    def hardened(value: Long): HardenedIndex = HardenedIndex(value + hardenedIndex)
  }

  case class PrivateKey(left: Array[Byte], right: Array[Byte], chainCode: Array[Byte]) {
    // Note: BigInt expects Big-Endian, but SLIP/BIP-ED25519 need Little-Endian
    lazy val leftNumber: BigInt = BigInt(1, left.reverse)
    lazy val rightNumber: BigInt = BigInt(1, right.reverse)

    def publicKey: PublicKey = {
      val ed25519 = new Ed25519
      val pk = new Array[Byte](ed25519.PUBLIC_KEY_SIZE)
      ed25519.scalarMultBaseEncoded(left, pk, 0)

      PublicKey(pk, chainCode)
    }

    def childPrivateKey(idx: Index): PrivateKey = {
      val z = idx match {
        case b: BasicIndex => hmac512WithKey2(chainCode, (0x00.toByte +: publicKey.bytes) ++ b.bytes)
        case h: HardenedIndex => hmac512WithKey2(chainCode, (0x02.toByte +: left) ++ h.bytes)
      }

      val nextKL = BigInt(1, z.slice(0, 28)) * 8 + leftNumber
      val nextKR = (BigInt(z.slice(32, 64)) + rightNumber) % (2 ^ 256)

      val nextChainCode = idx match {
        case b: BasicIndex => hmac512WithKey2(chainCode, (0x03.toByte +: publicKey.bytes) ++ b.bytes)
        case h: HardenedIndex => hmac512WithKey2(chainCode, (0x01.toByte +: right) ++ h.bytes)
      }

      PrivateKey(nextKL.toByteArray.tail.reverse, nextKR.toByteArray.tail.reverse, nextChainCode)
    }
  }

  case class HardenedIndexFailure()

  case class PublicKey(bytes: Array[Byte], chainCode: Array[Byte]) {
    lazy val addressNumber: BigInt = BigInt(1, bytes.reverse)

    def childPublicKey(idx: BasicIndex): PublicKey = {
      val z = hmac512WithKey2(chainCode, (0x02.toByte +: bytes) ++ idx.bytes)

      val zL = z.slice(0, 28)

      val nextPk = addressNumber + BigInt(1, zL.reverse)

      val nextChainCode = hmac512WithKey2(chainCode, (0x03.toByte +: bytes) ++ idx.bytes)

      PublicKey(nextPk.toByteArray.tail.reverse, nextChainCode)
    }

    def childPublicKey(idx: HardenedIndex, privateKey: PrivateKey): PublicKey = {
      val z = hmac512WithKey2(chainCode, (0x00.toByte +: (privateKey.left ++ privateKey.right)) ++ idx.bytes)

      val zL = z.slice(0, 28)

      val nextPk = addressNumber + (BigInt(1, zL.reverse) * 8)

      val nextChainCode = hmac512WithKey2(chainCode, (0x01.toByte +: (privateKey.left ++ privateKey.right)) ++ idx.bytes)

      PublicKey(nextPk.toByteArray.tail.reverse, nextChainCode)
    }
  }

  def fromMnemonic(m: Mnemonic, password: Option[String]): PrivateKey = fromSeed(m(password))

  def fromSeed(seed: Array[Byte]): PrivateKey = {
    val i = hmac512WithKey2("ed25519 cardano seed".getBytes(StandardCharsets.UTF_8), seed)
    val iL = i.slice(0, 32)
    val iR = i.slice(32, 64)
    val k = sha512.hash(iL).value
    k(0) = (k(0) & 0xf8).toByte
    k(31) = ((k(31) & 0x1f) | 0x40).toByte

    PrivateKey(k.slice(0, 32), k.slice(32, 64), iR)
  }

  private def hmac512WithKey2(key: Array[Byte], data: Array[Byte]): Array[Byte] = {
    val mac = new HMac(new SHA512Digest())
    mac.init(new KeyParameter(key))
    mac.update(data, 0, data.length)
    val out = new Array[Byte](64)
    mac.doFinal(out, 0)
    out
  }

  trait Instances {
    implicit val privateKeyAsBytes: AsBytes[Infallible, PrivateKey] = AsBytes.infallible(p => p.left ++ p.right)

    implicit val publicKeyBytes: AsBytes[Infallible, PublicKey] = AsBytes.infallible(p => p.bytes)
  }

  object implicits extends Instances
}
