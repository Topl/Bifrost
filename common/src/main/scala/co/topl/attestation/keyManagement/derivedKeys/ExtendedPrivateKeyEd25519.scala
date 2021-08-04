package co.topl.attestation.keyManagement.derivedKeys

import co.topl.attestation.keyManagement.mnemonicSeed.Mnemonic.Mnemonic
import co.topl.attestation.{PublicKeyPropositionEd25519, SignatureEd25519}
import co.topl.crypto.PublicKey
import co.topl.crypto.hash.sha512
import co.topl.crypto.signatures.{Ed25519, Signature}
import co.topl.utils.SizedByteCollection
import co.topl.utils.SizedByteCollection.Types.{ByteVector28, ByteVector32}
import co.topl.utils.SizedByteCollection.implicits._
import co.topl.utils.serialization.BifrostSerializer
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
class ExtendedPrivateKeyEd25519(
  val leftKey:   ByteVector32,
  val rightKey:  ByteVector32,
  val chainCode: ByteVector32,
  val path:      Seq[DerivedKeyIndex]
) {

  // Note: BigInt expects Big-Endian, but SLIP/BIP-ED25519 need Little-Endian
  val leftNumber: BigInt = BigInt(1, leftKey.toArray.reverse)
  val rightNumber: BigInt = BigInt(1, rightKey.toArray.reverse)

  /**
   * Gets the public key paired with this private key.
   * @return a `ExtendedPublicKeyEd25519` for verifying signatures made with this private key
   */
  def public: ExtendedPublicKeyEd25519 = {
    val ed25519 = new Ed25519
    val pk = new Array[Byte](ed25519.PUBLIC_KEY_SIZE)
    ed25519.scalarMultBaseEncoded(leftKey.toArray, pk, 0)

    ExtendedPublicKeyEd25519(
      SizedByteCollection[ByteVector32].fit(pk, ByteOrdering.LittleEndian),
      chainCode
    )
  }

  /**
   * Deterministically derives a child `ExtendedPrivateKey` at the given index from this extended private key.
   *
   * Follows section V.C from BIP32-ED25519 spec.
   *
   * @param index the index of the child key to derive
   * @return the derived `ExtendedPrivateKey`
   */
  def derive(
    index: DerivedKeyIndex
  ): Either[ExtendedPrivateKeyEd25519.InvalidDerivedKey, ExtendedPrivateKeyEd25519] = {
    val z = index match {
      case s: SoftIndex =>
        hmac512WithKey(
          chainCode.toVector,
          ByteVector(0x02.toByte) ++ public.bytes.toVector ++ s.bytes.toVector
        )
      case h: HardenedIndex =>
        hmac512WithKey(
          chainCode.toVector,
          ByteVector(0x00.toByte) ++ leftKey.toVector ++ rightKey.toVector ++ h.bytes.toVector
        )
    }

    val zLeft =
      BigInt(
        1,
        SizedByteCollection[ByteVector28].fit(z.slice(0, 28), ByteOrdering.LittleEndian).toArray.reverse
      )

    val zRight =
      BigInt(
        1,
        SizedByteCollection[ByteVector32].fit(z.slice(32, 64), ByteOrdering.LittleEndian).toArray.reverse
      )

    val nextLeft =
      SizedByteCollection[ByteVector32].fit(
        ByteBuffer
          .wrap(
            (zLeft * 8 + leftNumber).toByteArray.reverse
          )
          .order(ByteOrder.LITTLE_ENDIAN)
      )

    val nextRight =
      SizedByteCollection[ByteVector32].fit(
        ByteBuffer
          .wrap(((zRight + rightNumber) % (BigInt(2).pow(256))).toByteArray.reverse)
          .order(ByteOrder.LITTLE_ENDIAN)
      )

    val nextChainCode =
      SizedByteCollection[ByteVector32].fit(
        (index match {
          case b: SoftIndex =>
            hmac512WithKey(
              chainCode.toVector,
              ByteVector(0x03.toByte) ++ public.bytes.toVector ++ b.bytes.toVector
            )
          case h: HardenedIndex =>
            hmac512WithKey(
              chainCode.toVector,
              ByteVector(0x01.toByte) ++ leftKey.toVector ++ rightKey.toVector ++ h.bytes.toVector
            )
        }).slice(32, 64),
        ByteOrdering.LittleEndian
      )

    ExtendedPrivateKeyEd25519.validate(ExtendedPrivateKeyEd25519(nextLeft, nextRight, nextChainCode, path :+ index))
  }

  /**
   * Signs a message using this private key.
   * @param message the message to sign serialized as a `MessageToSign`
   * @return the signature
   */
  def sign(message: Array[Byte]): SignatureEd25519 = {
    // signing is a mutable process
    val mutableKey: ExtendedPrivateKeyEd25519 = ExtendedPrivateKeyEd25519.copy(this)
    val ec = new Ed25519

    val resultSig = new Array[Byte](ec.SIGNATURE_SIZE)
    val ctx: Array[Byte] = Array.empty
    val phflag: Byte = 0x00

    val h: Array[Byte] = mutableKey.leftKey.toArray ++ mutableKey.rightKey.toArray
    val s: Array[Byte] = mutableKey.leftKey.toArray
    val pk: Array[Byte] = mutableKey.public.bytes.toArray
    val m: Array[Byte] = message

    ec.implSign(ec.shaDigest, h, s, pk, 0, ctx, phflag, m, 0, m.length, resultSig, 0)

    SignatureEd25519(Signature(resultSig))
  }

  lazy val serializer: BifrostSerializer[ExtendedPrivateKeyEd25519] = ExtendedPrivateKeyEd25519Serializer

  lazy val publicImage: PublicKeyPropositionEd25519 = PublicKeyPropositionEd25519(PublicKey(public.bytes.toArray))
}

object ExtendedPrivateKeyEd25519 {

  case object InvalidDerivedKey
  type InvalidDerivedKey = InvalidDerivedKey.type

  /**
   * ED-25519 Base Order N
   *
   * Equivalent to `2^252 + 27742317777372353535851937790883648493`
   */
  val edBaseN: BigInt = BigInt("7237005577332262213973186563042994240857116359379907606001950938285454250989")

  def apply(
    leftKey:   ByteVector32,
    rightKey:  ByteVector32,
    chainCode: ByteVector32,
    path:      Seq[DerivedKeyIndex]
  ): ExtendedPrivateKeyEd25519 =
    new ExtendedPrivateKeyEd25519(leftKey, rightKey, chainCode, path)

  /**
   * Creates a copy of an `ExtendedPrivateKey`.
   * @param value the private key to copy
   * @return the copied `ExtendedPrivateKey`
   */
  def copy(value: ExtendedPrivateKeyEd25519): ExtendedPrivateKeyEd25519 =
    ExtendedPrivateKeyEd25519(
      SizedByteCollection[ByteVector32].fit(value.leftKey.toArray.clone(), ByteOrdering.LittleEndian),
      SizedByteCollection[ByteVector32].fit(value.rightKey.toArray.clone(), ByteOrdering.LittleEndian),
      SizedByteCollection[ByteVector32].fit(value.chainCode.toArray.clone(), ByteOrdering.LittleEndian),
      value.path
    )

  /**
   * Validates that the given key is a valid derived key.
   * Keys are invalid if their left private keys are divisible by the ED-25519 Base Order N.
   * @param value the private key value
   * @return either an invalid error or the private key
   */
  def validate(value: ExtendedPrivateKeyEd25519): Either[InvalidDerivedKey, ExtendedPrivateKeyEd25519] =
    Either.cond(value.leftNumber % edBaseN != 0, value, InvalidDerivedKey)
}
