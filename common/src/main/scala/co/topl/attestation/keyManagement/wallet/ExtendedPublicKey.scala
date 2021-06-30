package co.topl.attestation.keyManagement.wallet

import co.topl.crypto.PublicKey
import co.topl.crypto.signatures.Ed25519
import co.topl.utils.SizedByteVector.implicits._
import co.topl.utils.SizedByteVector
import co.topl.utils.SizedByteVector.Types.ByteVector32
import scodec.bits.ByteOrdering

class ExtendedPublicKey(private[wallet] val bytes: ByteVector32, val chainCode: ByteVector32) {
  private val publicKeyBytesAsNumber = BigInt(1, bytes.toArray.reverse)

  def derive(idx: SoftIndex): ExtendedPublicKey = {
    val z = hmac512WithKey(chainCode.toVector, (0x02.toByte +: bytes.toVector) ++ idx.bytes.toVector)

    val zL = z.slice(0, 28)

    val ed = new Ed25519
    val scaledZL = new Array[Byte](ed.PUBLIC_KEY_SIZE)
    val zLMult8 = SizedByteVector[ByteVector32].fit((8 * BigInt(1, zL.reverse.toArray)).toByteArray.reverse, ByteOrdering.LittleEndian)

    ed.scalarMultBaseEncoded(zLMult8.toArray, scaledZL, 0)

    val nextPkBytes = new Array[Byte](ed.PUBLIC_KEY_SIZE)
    val parentPublicKeyPoint = new ed.PointExt
    val scaledZLPoint = new ed.PointExt

    ed.decodePointVar(bytes.toArray, 0, negate = false, parentPublicKeyPoint)
    ed.decodePointVar(scaledZL, 0, negate = false, scaledZLPoint)

    val outputPoint = new ed.PointExt

    ed.pointAddVar(negate = false, parentPublicKeyPoint, scaledZLPoint, outputPoint)

    ed.encodePoint(outputPoint, nextPkBytes, 0)

    val nextPk =
      SizedByteVector[ByteVector32].fit(
        (publicKeyBytesAsNumber + BigInt(1, scaledZL)).toByteArray.reverse,
        ByteOrdering.LittleEndian
      )

    val nextChainCode =
      SizedByteVector[ByteVector32].fit(
        hmac512WithKey(chainCode.toVector, (0x03.toByte +: bytes.toVector) ++ idx.bytes.toVector),
        ByteOrdering.LittleEndian
      )

    ExtendedPublicKey(nextPk, nextChainCode)
  }

  def derive(index: HardenedIndex, privateKey: ExtendedPrivateKey): ExtendedPublicKey = {
    val z = hmac512WithKey(
      chainCode.toVector,
      (0x00.toByte +: (privateKey.leftKey.toVector ++ privateKey.rightKey.toVector)) ++ index.bytes.toVector
    )

    val zL = z.slice(0, 28)

    val nextPk =
      SizedByteVector[ByteVector32].fit(
        (publicKeyBytesAsNumber + (BigInt(1, zL.reverse.toArray) * 8)).toByteArray.reverse,
        ByteOrdering.LittleEndian
      )

    val nextChainCode =
      SizedByteVector[ByteVector32].fit(
        hmac512WithKey(
          chainCode.toVector,
          (0x01.toByte +: (privateKey.leftKey.toVector ++ privateKey.rightKey.toVector)) ++ index.bytes.toVector
        ),
        ByteOrdering.LittleEndian
      )

    ExtendedPublicKey(nextPk, nextChainCode)
  }

  def toPublicKey: PublicKey = PublicKey(bytes.toArray)
}

object ExtendedPublicKey {
  private[wallet] def apply(bytes: ByteVector32, chaincode: ByteVector32): ExtendedPublicKey =
    new ExtendedPublicKey(bytes, chaincode)
}
