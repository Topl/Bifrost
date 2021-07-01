package co.topl.attestation.keyManagement.wallet

import co.topl.attestation.SignatureEd25519
import co.topl.attestation.keyManagement.wallet.mnemonicSeed.Mnemonic.Mnemonic
import co.topl.crypto.PublicKey
import co.topl.crypto.hash.sha512
import co.topl.crypto.signatures.{Ed25519, MessageToSign, Signature}
import co.topl.utils.SizedByteVector.implicits._
import co.topl.utils.SizedByteVector
import co.topl.utils.SizedByteVector.Types.{ByteVector28, ByteVector32}
import scodec.bits.{ByteOrdering, ByteVector}

import java.nio.charset.StandardCharsets
import java.nio.{ByteBuffer, ByteOrder}

/**
 * An ED-25519 extended private key.
 *
 * See https://drive.google.com/file/d/0ByMtMw2hul0EMFJuNnZORDR2NDA/view?resourcekey=0-5yJh8wV--HB7ve2NuqfQ6A
 * for the BIP32-ED25519 specification
 *
 * @param leftKey the left 32 bytes of the private key
 * @param rightKey the right 32 bytes of the private key
 * @param chainCode the 32-byte chain code
 */
class ExtendedPrivateKey(
  private[wallet] val leftKey:   ByteVector32,
  private[wallet] val rightKey:  ByteVector32,
  private[wallet] val chainCode: ByteVector32
) {

  // Note: BigInt expects Big-Endian, but SLIP/BIP-ED25519 need Little-Endian
  private[wallet] val leftNumber: BigInt = BigInt(1, leftKey.toArray.reverse)
  private[wallet] val rightNumber: BigInt = BigInt(1, rightKey.toArray.reverse)

  /**
   * Gets the public key paired with this private key.
   * @return a `PublicKey` for verifying signatures made with this private key
   */
  def publicKey: PublicKey = {
    val ed25519 = new Ed25519
    val pk = new Array[Byte](ed25519.PUBLIC_KEY_SIZE)
    ed25519.scalarMultBaseEncoded(leftKey.toArray, pk, 0)

    PublicKey(pk)
  }

  /**
   * Deterministically derives a child `ExtendedPrivateKey` at the given index from this extended private key.
   * @param index the index of the child key to derive
   * @return the derived `ExtendedPrivateKey`
   */
  def derive(index: DerivedKeyIndex): ExtendedPrivateKey = {
    val z = index match {
      case s: SoftIndex =>
        hmac512WithKey(chainCode.toVector, ByteVector(0x00.toByte) ++ ByteVector(publicKey.value) ++ s.bytes.toVector)
      case h: HardenedIndex =>
        hmac512WithKey(
          chainCode.toVector,
          ByteVector(0x02.toByte) ++ leftKey.toVector ++ rightKey.toVector ++ h.bytes.toVector
        )
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
            hmac512WithKey(
              chainCode.toVector,
              ByteVector(0x03.toByte) ++ ByteVector(publicKey.value) ++ b.bytes.toVector
            )
          case h: HardenedIndex =>
            hmac512WithKey(chainCode.toVector, ByteVector(0x01.toByte) ++ rightKey.toVector ++ h.bytes.toVector)
        },
        ByteOrdering.LittleEndian
      )

    ExtendedPrivateKey(nextLeft, nextRight, nextChainCode)
  }

  /**
   * Signs a message using this private key.
   * @param message the message to sign serialized as a `MessageToSign`
   * @return the signature
   */
  def sign(message: MessageToSign): SignatureEd25519 = {
    // signing is a mutable process
    val mutableKey: ExtendedPrivateKey = ExtendedPrivateKey.copy(this)
    val ec = new Ed25519

    val resultSig = new Array[Byte](ec.SIGNATURE_SIZE)
    val ctx: Array[Byte] = Array.empty
    val phflag: Byte = 0x00

    val h: Array[Byte] = mutableKey.leftKey.toArray ++ mutableKey.rightKey.toArray
    val s: Array[Byte] = mutableKey.leftKey.toArray
    val pk: Array[Byte] = mutableKey.publicKey.value
    val m: Array[Byte] = message

    ec.implSign(ec.shaDigest, h, s, pk, 0, ctx, phflag, m, 0, m.length, resultSig, 0)

    SignatureEd25519(Signature(resultSig))
  }
}

object ExtendedPrivateKey {

  /**
   * Creates a new `ExtendedPrivateKey` instance from the key and chain code.
   * @param leftKeyBytes the left 32-bytes of the private key
   * @param rightKeyBytes the left 32-bytes of the private key
   * @param chaincode the 32-byte chain code
   * @return a new `ExtendedPrivateKey` instance
   */
  private[wallet] def apply(
    leftKeyBytes:  ByteVector32,
    rightKeyBytes: ByteVector32,
    chaincode:     ByteVector32
  ): ExtendedPrivateKey =
    new ExtendedPrivateKey(leftKeyBytes, rightKeyBytes, chaincode)

  /**
   * Generates a root `ExtendedPrivateKey` from a mnemonic phrase and an optional password.
   * @param m the mnemonic phrase
   * @param password the password (optional)
   * @return a root `ExtendedPrivateKey`
   */
  def fromMnemonic(m: Mnemonic, password: Option[String]): ExtendedPrivateKey = fromSeed(m(password))

  /**
   * Creates a root `ExtendedPrivateKey` from the given seed.
   * See https://github.com/satoshilabs/slips/blob/master/slip-0023.md
   * for specification.
   * @param seed the seed to generate from
   * @return a root `ExtendedPrivateKey`
   */
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

  /**
   * Creates a copy of an `ExtendedPrivateKey`.
   * @param value the private key to copy
   * @return the copied `ExtendedPrivateKey`
   */
  private[wallet] def copy(value: ExtendedPrivateKey): ExtendedPrivateKey =
    new ExtendedPrivateKey(
      SizedByteVector[ByteVector32].fit(value.leftKey.toArray.clone(), ByteOrdering.LittleEndian),
      SizedByteVector[ByteVector32].fit(value.rightKey.toArray.clone(), ByteOrdering.LittleEndian),
      SizedByteVector[ByteVector32].fit(value.chainCode.toArray.clone(), ByteOrdering.LittleEndian)
    )
}
