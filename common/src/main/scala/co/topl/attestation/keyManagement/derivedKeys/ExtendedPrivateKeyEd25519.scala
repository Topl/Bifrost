package co.topl.attestation.keyManagement.derivedKeys

import co.topl.attestation.keyManagement.mnemonic.{Entropy, FromEntropy}
import co.topl.attestation.{PublicKeyPropositionEd25519, SignatureEd25519}
import co.topl.crypto.signatures.Signature
import co.topl.crypto.signing.Ed25519
import co.topl.crypto.{Pbkdf2Sha512, PublicKey}
import co.topl.utils.SizedBytes
import co.topl.utils.SizedBytes.Types.{ByteVector28, ByteVector32, ByteVector96}
import co.topl.utils.SizedBytes.implicits._
import co.topl.utils.serialization.BifrostSerializer
import scodec.bits.ByteOrdering.LittleEndian
import scodec.bits.{ByteOrdering, ByteVector}

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
  val chainCode: ByteVector32
) {

  import ExtendedPrivateKeyEd25519.ed25519

  // Note: BigInt expects Big-Endian, but SLIP/BIP-ED25519 need Little-Endian
  val leftNumber: BigInt = BigInt(1, leftKey.toArray.reverse)
  val rightNumber: BigInt = BigInt(1, rightKey.toArray.reverse)

  /**
   * Gets the public key paired with this private key.
   * @return a `ExtendedPublicKeyEd25519` for verifying signatures made with this private key
   */
  def public: ExtendedPublicKeyEd25519 = {
    val pk = new Array[Byte](ed25519.PUBLIC_KEY_SIZE)
    ed25519.scalarMultBaseEncoded(leftKey.toArray, pk, 0)

    ExtendedPublicKeyEd25519(
      SizedBytes[ByteVector32].fit(pk, ByteOrdering.LittleEndian),
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
        SizedBytes[ByteVector28].fit(z.slice(0, 28), ByteOrdering.LittleEndian).toArray.reverse
      )

    val zRight =
      BigInt(
        1,
        SizedBytes[ByteVector32].fit(z.slice(32, 64), ByteOrdering.LittleEndian).toArray.reverse
      )

    val nextLeft =
      SizedBytes[ByteVector32].fit(
        ByteBuffer
          .wrap(
            (zLeft * 8 + leftNumber).toByteArray.reverse
          )
          .order(ByteOrder.LITTLE_ENDIAN)
      )

    val nextRight =
      SizedBytes[ByteVector32].fit(
        ByteBuffer
          .wrap(((zRight + rightNumber) % (BigInt(2).pow(256))).toByteArray.reverse)
          .order(ByteOrder.LITTLE_ENDIAN)
      )

    val nextChainCode =
      SizedBytes[ByteVector32].fit(
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

    ExtendedPrivateKeyEd25519.validate(ExtendedPrivateKeyEd25519(nextLeft, nextRight, nextChainCode))
  }

  /**
   * Signs a message using this private key.
   * @param message the message to sign serialized as a `MessageToSign`
   * @return the signature
   */
  def sign(message: Array[Byte]): Signature = {
    // signing is a mutable process
    val mutableKey: ExtendedPrivateKeyEd25519 = ExtendedPrivateKeyEd25519.copy(this)

    val resultSig = new Array[Byte](ed25519.SIGNATURE_SIZE)
    val ctx: Array[Byte] = Array.empty
    val phflag: Byte = 0x00

    val h: Array[Byte] = mutableKey.leftKey.toArray ++ mutableKey.rightKey.toArray
    val s: Array[Byte] = mutableKey.leftKey.toArray
    val pk: Array[Byte] = mutableKey.public.bytes.toArray
    val m: Array[Byte] = message

    ed25519.implSign(ed25519.sha512Digest, h, s, pk, 0, ctx, phflag, m, 0, m.length, resultSig, 0)

    Signature(resultSig)
  }

  lazy val serializer: BifrostSerializer[ExtendedPrivateKeyEd25519] = ExtendedPrivateKeyEd25519Serializer

  lazy val publicImage: PublicKeyPropositionEd25519 = PublicKeyPropositionEd25519(PublicKey(public.bytes.toArray))
}

object ExtendedPrivateKeyEd25519 {
  val ed25519 = new Ed25519
  case object InvalidDerivedKey
  type InvalidDerivedKey = InvalidDerivedKey.type

  /**
   * The type of the password used alongside a mnemonic to generate an `ExtendedPrivateKeyEd25519`
   */
  type Password = String

  /**
   * ED-25519 Base Order N
   *
   * Equivalent to `2^252 + 27742317777372353535851937790883648493`
   */
  val edBaseN: BigInt = BigInt("7237005577332262213973186563042994240857116359379907606001950938285454250989")

  /**
   * Instantiates an `ExtendedPrivateKeyEd25519` from a 32-byte left/right key and 32-byte chain code
   * with little-endian ordering
   * @param leftKey the 32-byte left key
   * @param rightKey the 32-byte right key
   * @param chainCode the 32-byte chain code
   * @return the `ExtendedPrivateKeyEd25519` representing the bytes
   */
  def apply(
    leftKey:   ByteVector32,
    rightKey:  ByteVector32,
    chainCode: ByteVector32
  ): ExtendedPrivateKeyEd25519 =
    new ExtendedPrivateKeyEd25519(leftKey, rightKey, chainCode)

  /**
   * Instantiates an `ExtendedPrivateKeyEd25519` from a 96-byte vector with little endian ordering.
   * @param bytes 96 bytes representing the left/right keys with a chain code in little-endian
   * @return an `ExtendedPrivateKeyEd25519` representing the bytes
   */
  def apply(bytes: ByteVector96): ExtendedPrivateKeyEd25519 =
    new ExtendedPrivateKeyEd25519(
      SizedBytes[ByteVector32].fit(bytes.value.slice(0, 32), ByteOrdering.LittleEndian),
      SizedBytes[ByteVector32].fit(bytes.value.slice(32, 64), ByteOrdering.LittleEndian),
      SizedBytes[ByteVector32].fit(bytes.value.slice(64, 96), ByteOrdering.LittleEndian)
    )

  /**
   * Instantiates an `ExtendedPrivateKeyEd25519` from entropy and a password.
   * @param entropy some random entropy
   * @param password an optional password for an extra layer of security
   * @return an `ExtendedPrivateKeyEd25519`
   */
  def apply(entropy: Entropy, password: Password): ExtendedPrivateKeyEd25519 = {
    // first do a PBDKF2-HMAC-SHA512 per the SLIP2-0023 spec
    val seed = Pbkdf2Sha512.generateKey(
      password.getBytes,
      entropy.value,
      96,
      4096
    )

    // turn seed into a valid ExtendedPrivateKeyEd25519 per the SLIP-0023 spec
    seed(0) = (seed(0) & 0xf8).toByte
    seed(31) = ((seed(31) & 0x1f) | 0x40).toByte

    ExtendedPrivateKeyEd25519(SizedBytes[ByteVector96].fit(seed, LittleEndian))
  }

  /**
   * Creates a copy of an `ExtendedPrivateKey`.
   * @param value the private key to copy
   * @return the copied `ExtendedPrivateKey`
   */
  def copy(value: ExtendedPrivateKeyEd25519): ExtendedPrivateKeyEd25519 =
    ExtendedPrivateKeyEd25519(
      SizedBytes[ByteVector32].fit(value.leftKey.toArray.clone(), ByteOrdering.LittleEndian),
      SizedBytes[ByteVector32].fit(value.rightKey.toArray.clone(), ByteOrdering.LittleEndian),
      SizedBytes[ByteVector32].fit(value.chainCode.toArray.clone(), ByteOrdering.LittleEndian)
    )

  /**
   * Validates that the given key is a valid derived key.
   * Keys are invalid if their left private keys are divisible by the ED-25519 Base Order N.
   * @param value the private key value
   * @return either an invalid error or the private key
   */
  def validate(value: ExtendedPrivateKeyEd25519): Either[InvalidDerivedKey, ExtendedPrivateKeyEd25519] =
    Either.cond(value.leftNumber % edBaseN != 0, value, InvalidDerivedKey)

  trait Instances {

    implicit def fromEntropy: FromEntropy[Password => ExtendedPrivateKeyEd25519] =
      entropy => password => ExtendedPrivateKeyEd25519(entropy, password)
  }

  object implicits extends Instances
}
