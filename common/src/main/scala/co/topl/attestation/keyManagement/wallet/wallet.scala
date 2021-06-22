package co.topl.attestation.keyManagement

import co.topl.attestation.{PublicKeyPropositionEd25519, SignatureEd25519}
import co.topl.attestation.keyManagement.wallet.bip39.Mnemonic.Mnemonic
import co.topl.crypto.PublicKey
import co.topl.crypto.hash.sha512
import org.bouncycastle.crypto.digests.SHA512Digest
import org.bouncycastle.crypto.macs.HMac
import org.bouncycastle.crypto.params.KeyParameter
import co.topl.crypto.signatures.eddsa.Ed25519
import co.topl.utils.SizedByteVector
import co.topl.utils.SizedByteVector.implicits._
import co.topl.utils.SizedByteVector.Types.{ByteVector28, ByteVector32, ByteVector4}
import co.topl.utils.codecs.{AsBytes, Infallible}
import scodec.bits.{ByteOrdering, ByteVector}

import java.nio.{ByteBuffer, ByteOrder}
import java.nio.charset.StandardCharsets
import scala.language.implicitConversions

// https://raw.githubusercontent.com/input-output-hk/adrestia/master/user-guide/static/Ed25519_BIP.pdf
package object wallet {

  sealed trait Index {

    val value: Int

    def bytes: ByteVector4 =
      SizedByteVector[ByteVector4]
        .fit(ByteBuffer.allocate(4).order(ByteOrder.LITTLE_ENDIAN).putInt(value))
  }

  case class SoftIndex private (override val value: Int) extends Index
  case class HardenedIndex private (override val value: Int) extends Index

  object Index {

    private val hardenedIndex: Int = 0x80000000

    def soft(value: Int): Option[SoftIndex] =
      if (value >= hardenedIndex) None
      else Some(SoftIndex(value))

    def hardened(value: Int): HardenedIndex = HardenedIndex(value + hardenedIndex)
  }

  case class ExtendedPrivateKey(left: ByteVector32, right: ByteVector32, chainCode: ByteVector32) {

    // Note: BigInt expects Big-Endian, but SLIP/BIP-ED25519 need Little-Endian
    val leftNumber: BigInt = BigInt(1, left.toArray.reverse)
    val rightNumber: BigInt = BigInt(1, right.toArray.reverse)

    def publicKey: ExtendedPublicKey = {
      val ed25519 = new Ed25519
      val pk = new Array[Byte](ed25519.PUBLIC_KEY_SIZE)
      ed25519.scalarMultBaseEncoded(left.toArray, pk, 0)

      ExtendedPublicKey(SizedByteVector[ByteVector32].fit(pk, ByteOrdering.LittleEndian), chainCode)
    }

    def derive(idx: Index): ExtendedPrivateKey = {
      val z = idx match {
        case s: SoftIndex =>
          hmac512WithKey(chainCode.toVector, (0x00.toByte +: publicKey.bytes.toVector) ++ s.bytes.toVector)
        case h: HardenedIndex =>
          hmac512WithKey(chainCode.toVector, (0x02.toByte +: (left.toVector ++ right.toVector)) ++ h.bytes.toVector)
      }

      val zLeft =
        BigInt(
          1, SizedByteVector[ByteVector28].fit(z.slice(0, 28), ByteOrdering.LittleEndian).toArray.reverse
        )

      val zRight =
        BigInt(
          1, SizedByteVector[ByteVector32].fit(z.slice(32, 64), ByteOrdering.LittleEndian).toArray.reverse
        )

      val nextLeft =
        SizedByteVector[ByteVector32].fit(
          ByteBuffer.wrap(
            (zLeft * 8 + leftNumber).toByteArray.reverse
          ).order(ByteOrder.LITTLE_ENDIAN)
        )

      val nextRight =
        SizedByteVector[ByteVector32].fit(
          ByteBuffer
            .wrap(((zRight + rightNumber) % (2 ^ 256)).toByteArray.reverse)
            .order(ByteOrder.LITTLE_ENDIAN)
        )

      val nextChainCode =
        SizedByteVector[ByteVector32].fit(
          idx match {
            case b: SoftIndex =>
              hmac512WithKey(chainCode.toVector, (0x03.toByte +: publicKey.bytes.toVector) ++ b.bytes.toVector)
            case h: HardenedIndex =>
              hmac512WithKey(chainCode.toVector, (0x01.toByte +: right.toVector) ++ h.bytes.toVector)
          },
          ByteOrdering.LittleEndian
        )

      ExtendedPrivateKey(nextLeft, nextRight, nextChainCode)
    }
  }

  case class HardenedIndexFailure()

  case class ExtendedPublicKey(bytes: ByteVector32, chainCode: ByteVector32) {
    private val asNumber = BigInt(1, bytes.toArray.reverse)

    def derive(idx: SoftIndex): ExtendedPublicKey = {
      val z = hmac512WithKey(chainCode.toVector, (0x02.toByte +: bytes.toVector) ++ idx.bytes.toVector)

      val zL = z.slice(0, 28)

      val nextPk =
        SizedByteVector[ByteVector32].fit(
          (asNumber + BigInt(1, zL.reverse.toArray)).toByteArray.reverse,
          ByteOrdering.LittleEndian
        )

      val nextChainCode =
        SizedByteVector[ByteVector32].fit(
          hmac512WithKey(chainCode.toVector, (0x03.toByte +: bytes.toVector) ++ idx.bytes.toVector),
          ByteOrdering.LittleEndian
        )

      ExtendedPublicKey(nextPk, nextChainCode)
    }

    def derive(idx: HardenedIndex, privateKey: ExtendedPrivateKey): ExtendedPublicKey = {
      val z = hmac512WithKey(
        chainCode.toVector,
        (0x00.toByte +: (privateKey.left.toVector ++ privateKey.right.toVector)) ++ idx.bytes.toVector
      )

      val zL = z.slice(0, 28)

      val nextPk =
        SizedByteVector[ByteVector32].fit(
          (asNumber + (BigInt(1, zL.reverse.toArray) * 8)).toByteArray.reverse,
          ByteOrdering.LittleEndian
        )

      val nextChainCode =
        SizedByteVector[ByteVector32].fit(
          hmac512WithKey(
            chainCode.toVector,
            (0x01.toByte +: (privateKey.left.toVector ++ privateKey.right.toVector)) ++ idx.bytes.toVector
          ),
          ByteOrdering.LittleEndian
        )

      ExtendedPublicKey(nextPk, nextChainCode)
    }

    def toProposition: PublicKeyPropositionEd25519 = new PublicKeyPropositionEd25519(PublicKey(bytes.toArray))
  }

  def fromMnemonic(m: Mnemonic, password: Option[String]): ExtendedPrivateKey = fromSeed(m(password))

  def fromSeed(seed: Array[Byte]): ExtendedPrivateKey = {
    val i =
      hmac512WithKey(ByteVector.view("ed25519 cardano seed".getBytes(StandardCharsets.UTF_8)), ByteVector.view(seed))

    val iLeft = i.slice(0, 32)
    val iRight = i.slice(32, 64)

    val k = sha512.hash(iLeft.toArray).value
    k(0) = (k(0) & 0xf8).toByte
    k(31) = ((k(31) & 0x1f) | 0x40).toByte

    ExtendedPrivateKey(
      SizedByteVector[ByteVector32].fit(k.slice(0, 32), ByteOrdering.LittleEndian),
      SizedByteVector[ByteVector32].fit(k.slice(32, 64), ByteOrdering.LittleEndian),
      SizedByteVector[ByteVector32].fit(iRight, ByteOrdering.LittleEndian)
    )
  }

  def sign(privateKey: ExtendedPrivateKey, message: Array[Byte]): SignatureEd25519 = ???

  private def hmac512WithKey(key: ByteVector, data: ByteVector): ByteVector = {
    val mac = new HMac(new SHA512Digest())
    mac.init(new KeyParameter(key.toArray))
    mac.update(data.toArray, 0, data.length.toInt)
    val out = new Array[Byte](64)
    mac.doFinal(out, 0)
    ByteVector.view(out)
  }

  trait Instances {

    implicit val privateKeyAsBytes: AsBytes[Infallible, ExtendedPrivateKey] =
      AsBytes.infallible(p => p.left.toArray ++ p.right.toArray)

    implicit val publicKeyBytes: AsBytes[Infallible, ExtendedPublicKey] = AsBytes.infallible(p => p.bytes.toArray)
  }

  object implicits extends Instances
}
