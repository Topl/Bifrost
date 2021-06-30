package co.topl.attestation.keyManagement.wallet

import co.topl.attestation.SignatureEd25519
import co.topl.attestation.keyManagement.wallet.ExtendedPrivateKey.copy
import co.topl.attestation.keyManagement.wallet.mnemonicSeed.Mnemonic.Mnemonic
import co.topl.crypto.hash.sha512
import co.topl.crypto.signatures.{Ed25519, MessageToSign, Signature}
import co.topl.utils.SizedByteVector.implicits._
import co.topl.utils.SizedByteVector
import co.topl.utils.SizedByteVector.Types.{ByteVector28, ByteVector32}
import scodec.bits.{ByteOrdering, ByteVector}

import java.nio.charset.StandardCharsets
import java.nio.{ByteBuffer, ByteOrder}

class ExtendedPrivateKey(
  private[wallet] val leftKey:   ByteVector32,
  private[wallet] val rightKey:  ByteVector32,
  val chainCode: ByteVector32
) {

  // Note: BigInt expects Big-Endian, but SLIP/BIP-ED25519 need Little-Endian
  private[wallet] val leftNumber: BigInt = BigInt(1, leftKey.toArray.reverse)
  private[wallet] val rightNumber: BigInt = BigInt(1, rightKey.toArray.reverse)

  def publicKey: ExtendedPublicKey = {
    val ed25519 = new Ed25519
    val pk = new Array[Byte](ed25519.PUBLIC_KEY_SIZE)
    ed25519.scalarMultBaseEncoded(leftKey.toArray, pk, 0)

    ExtendedPublicKey(SizedByteVector[ByteVector32].fit(pk, ByteOrdering.LittleEndian), chainCode)
  }

  def derive(index: DerivedKeyIndex): ExtendedPrivateKey = {
    val z = index match {
      case s: SoftIndex =>
        hmac512WithKey(chainCode.toVector, (0x00.toByte +: publicKey.bytes.toVector) ++ s.bytes.toVector)
      case h: HardenedIndex =>
        hmac512WithKey(chainCode.toVector, (0x02.toByte +: (leftKey.toVector ++ rightKey.toVector)) ++ h.bytes.toVector)
    }

    val zLeft =
      BigInt(
        1,
        SizedByteVector[ByteVector28].fit(z.slice(0, 28), ByteOrdering.LittleEndian).toArray.reverse
      )

    val zRight =
      BigInt(
        1,
        SizedByteVector[ByteVector32].fit(z.slice(32, 64), ByteOrdering.LittleEndian).toArray.reverse
      )

    val nextLeft =
      SizedByteVector[ByteVector32].fit(
        ByteBuffer
          .wrap(
            (zLeft * 8 + leftNumber).toByteArray.reverse
          )
          .order(ByteOrder.LITTLE_ENDIAN)
      )

    val nextRight =
      SizedByteVector[ByteVector32].fit(
        ByteBuffer
          .wrap(((zRight + rightNumber) % (2 ^ 256)).toByteArray.reverse)
          .order(ByteOrder.LITTLE_ENDIAN)
      )

    val nextChainCode =
      SizedByteVector[ByteVector32].fit(
        index match {
          case b: SoftIndex =>
            hmac512WithKey(chainCode.toVector, (0x03.toByte +: publicKey.bytes.toVector) ++ b.bytes.toVector)
          case h: HardenedIndex =>
            hmac512WithKey(chainCode.toVector, (0x01.toByte +: rightKey.toVector) ++ h.bytes.toVector)
        },
        ByteOrdering.LittleEndian
      )

    ExtendedPrivateKey(nextLeft, nextRight, nextChainCode)
  }

  def sign(message: MessageToSign): SignatureEd25519 = {
    // signing is a mutable process
    val mutableKey: ExtendedPrivateKey = copy(this)
    val ec = new Ed25519

    val resultSig = new Array[Byte](ec.SIGNATURE_SIZE)
    val ctx: Array[Byte] = Array.empty
    val phflag: Byte = 0x00

    val h: Array[Byte] = mutableKey.leftKey.toArray ++ mutableKey.rightKey.toArray
    val s: Array[Byte] = mutableKey.leftKey.toArray
    val pk: Array[Byte] = mutableKey.publicKey.bytes.toArray
    val m: Array[Byte] = message

    ec.implSign(ec.shaDigest, h, s, pk, 0, ctx, phflag, m, 0, m.length, resultSig, 0)

    SignatureEd25519(Signature(resultSig))
  }
}

object ExtendedPrivateKey {

  private[wallet] def apply(
    leftKeyBytes:  ByteVector32,
    rightKeyBytes: ByteVector32,
    chaincode:     ByteVector32
  ): ExtendedPrivateKey =
    new ExtendedPrivateKey(leftKeyBytes, rightKeyBytes, chaincode)

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

  private[wallet] def copy(value: ExtendedPrivateKey): ExtendedPrivateKey =
    new ExtendedPrivateKey(
      SizedByteVector[ByteVector32].fit(value.leftKey.toArray.clone(), ByteOrdering.LittleEndian),
      SizedByteVector[ByteVector32].fit(value.rightKey.toArray.clone(), ByteOrdering.LittleEndian),
      SizedByteVector[ByteVector32].fit(value.chainCode.toArray.clone(), ByteOrdering.LittleEndian)
    )
}
