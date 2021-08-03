package co.topl.attestation.keyManagement.derivedKeys

import co.topl.crypto.PublicKey
import co.topl.crypto.signatures.Ed25519
import co.topl.utils.SizedByteCollection
import co.topl.utils.SizedByteCollection.implicits._
import co.topl.utils.SizedByteCollection.Types.ByteVector32
import scodec.bits.ByteOrdering

/**
 * An ED-25519 Extended public key.
 *
 * See https://drive.google.com/file/d/0ByMtMw2hul0EMFJuNnZORDR2NDA/view?resourcekey=0-5yJh8wV--HB7ve2NuqfQ6A
 * for the BIP32-ED25519 specification
 *
 * @param bytes the bytes representing the verification key
 * @param chainCode the chaincode which matches the private key to the public key
 */
class ExtendedPublicKeyEd25519(val bytes: ByteVector32, val chainCode: ByteVector32) {

  /**
   * Deterministically derives a child public key located at the given soft index.
   *
   * Follows section V.D from the BIP32-ED25519 spec.
   *
   * @param index the index of the key to derive
   * @return an extended public key
   */
  def derive(index: SoftIndex): ExtendedPublicKeyEd25519 = {
    val z = hmac512WithKey(chainCode.toVector, (0x02.toByte +: bytes.toVector) ++ index.bytes.toVector)

    val zL = z.slice(0, 28)

    val ed = new Ed25519

    val zLMult8 = SizedByteCollection[ByteVector32].fit(
      (8 * BigInt(1, zL.reverse.toArray)).toByteArray.reverse,
      ByteOrdering.LittleEndian
    )

    val scaledZL = new ed.PointAccum
    ed.scalarMultBase(zLMult8.toArray, scaledZL)

    val publicKeyPoint = new ed.PointExt
    ed.decodePointVar(bytes.toArray, 0, negate = false, publicKeyPoint)

    ed.pointAddVar(negate = false, publicKeyPoint, scaledZL)

    val nextPublicKeyBytes = new Array[Byte](ed.KeyLength)
    ed.encodePoint(scaledZL, nextPublicKeyBytes, 0)

    val nextPk = SizedByteCollection[ByteVector32].fit(nextPublicKeyBytes, ByteOrdering.LittleEndian)

    val nextChainCode =
      SizedByteCollection[ByteVector32].fit(
        hmac512WithKey(chainCode.toVector, (0x03.toByte +: bytes.toVector) ++ index.bytes.toVector)
          .slice(32, 64),
        ByteOrdering.LittleEndian
      )

    ExtendedPublicKeyEd25519(nextPk, nextChainCode)
  }

  /**
   * Gets the `PublicKey` from this `ExtendedPublicKeyEd25519` value.
   * @return the Ed25519 public key as a `PublicKey`
   */
  def toPublicKey: PublicKey = PublicKey(bytes.toArray)
}

object ExtendedPublicKeyEd25519 {

  def apply(bytes: ByteVector32, chaincode: ByteVector32): ExtendedPublicKeyEd25519 =
    new ExtendedPublicKeyEd25519(bytes, chaincode)
}
