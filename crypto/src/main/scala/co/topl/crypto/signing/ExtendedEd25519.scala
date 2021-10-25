package co.topl.crypto.signing

import co.topl.crypto.Pbkdf2Sha512
import co.topl.crypto.mnemonic.{Bip32Index, Bip32Indexes, Entropy}
import co.topl.models._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Sized
import org.bouncycastle.crypto.digests.SHA512Digest
import org.bouncycastle.crypto.macs.HMac
import org.bouncycastle.crypto.params.KeyParameter

import java.nio.charset.StandardCharsets
import java.nio.{ByteBuffer, ByteOrder}

class ExtendedEd25519
    extends eddsa.Ed25519
    with EllipticCurveSignatureScheme[
      SecretKeys.ExtendedEd25519,
      VerificationKeys.ExtendedEd25519,
      Proofs.Signature.Ed25519
    ] {
  override val SignatureLength: Int = SIGNATURE_SIZE
  override val KeyLength: Int = SECRET_KEY_SIZE
  val PublicKeyLength: Int = PUBLIC_KEY_SIZE

  override def createKeyPair(seed: Bytes): (SecretKeys.ExtendedEd25519, VerificationKeys.ExtendedEd25519) = {
    val sk = ExtendedEd25519.fromEntropy(Entropy(seed.toArray))("")
    val vk = generatePublicKey(sk)
    (sk, vk)
  }

  override def sign(privateKey: SecretKeys.ExtendedEd25519, message: Bytes): Proofs.Signature.Ed25519 = {
    val resultSig = new Array[Byte](SIGNATURE_SIZE)
    val pk: Array[Byte] = new Array[Byte](PUBLIC_KEY_SIZE)
    val ctx: Array[Byte] = Array.empty
    val phflag: Byte = 0x00
    val h: Array[Byte] = privateKey.leftKey.data.toArray ++ privateKey.rightKey.data.toArray
    val s: Array[Byte] = privateKey.leftKey.data.toArray
    val m: Array[Byte] = message.toArray

    scalarMultBaseEncoded(privateKey.leftKey.data.toArray, pk, 0)
    implSign(sha512Digest, h, s, pk, 0, ctx, phflag, m, 0, m.length, resultSig, 0)

    Proofs.Signature.Ed25519(Sized.strictUnsafe(Bytes(resultSig)))
  }

  def verify(signature: Proofs.Signature.Ed25519, message: Bytes, verifyKey: VerificationKeys.Ed25519): Boolean =
    verifyKey.bytes.data.length == PUBLIC_KEY_SIZE &&
    signature.bytes.data.length == SIGNATURE_SIZE &&
    verify(
      Bytes.toByteArray(signature.bytes.data),
      0,
      Bytes.toByteArray(verifyKey.bytes.data),
      0,
      message.toArray,
      0,
      message.toArray.length
    )

  override def verify(
    signature: Proofs.Signature.Ed25519,
    message:   Bytes,
    verifyKey: VerificationKeys.ExtendedEd25519
  ): Boolean =
    signature.bytes.data.length == SIGNATURE_SIZE &&
    verifyKey.ed25519.bytes.data.length == PUBLIC_KEY_SIZE &&
    verify(
      signature.bytes.data.toArray,
      0,
      verifyKey.ed25519.bytes.data.toArray,
      0,
      message.toArray,
      0,
      message.toArray.length
    )

  def deriveSecret(
    secretKey: SecretKeys.ExtendedEd25519,
    index:     Bip32Index
  ): SecretKeys.ExtendedEd25519 = {

    val lNum: BigInt = ExtendedEd25519.leftNumber(secretKey)
    val rNum: BigInt = ExtendedEd25519.rightNumber(secretKey)
    val public: VerificationKeys.ExtendedEd25519 = generatePublicKey(secretKey)

    val zHmacData: Bytes = index match {
      case _: Bip32Indexes.SoftIndex =>
        0x02.toByte +: (public.ed25519.bytes.data ++ index.bytes.data)
      case _: Bip32Indexes.HardenedIndex =>
        0x00.toByte +: (secretKey.leftKey.data ++ secretKey.rightKey.data ++ index.bytes.data)
    }
    val z = ExtendedEd25519.hmac512WithKey(secretKey.chainCode.data.toArray, zHmacData.toArray)

    val zLeft =
      BigInt(1, z.slice(0, 28).reverse.toArray)

    val zRight =
      BigInt(1, z.slice(32, 64).reverse.toArray)

    val nextLeft =
      Bytes(
        ByteBuffer
          .wrap((zLeft * 8 + lNum).toByteArray.reverse)
          .order(ByteOrder.LITTLE_ENDIAN)
          .array()
          .take(32)
      )

    val nextRight =
      Bytes(
        ByteBuffer
          .wrap(((zRight + rNum) % (BigInt(2).pow(256))).toByteArray.reverse)
          .order(ByteOrder.LITTLE_ENDIAN)
          .array()
          .take(32)
      )

    val chaincodeHmacData = index match {
      case _: Bip32Indexes.SoftIndex =>
        0x03.toByte +: (public.ed25519.bytes.data ++ index.bytes.data)
      case _: Bip32Indexes.HardenedIndex =>
        0x01.toByte +: (secretKey.leftKey.data ++ secretKey.rightKey.data ++ index.bytes.data)
    }

    val nextChainCode =
      Bytes(
        ExtendedEd25519
          .hmac512WithKey(secretKey.chainCode.data.toArray, chaincodeHmacData.toArray)
          .slice(32, 64)
          .toArray
      )

    SecretKeys.ExtendedEd25519(
      Sized.strictUnsafe(nextLeft),
      Sized.strictUnsafe(nextRight),
      Sized.strictUnsafe(nextChainCode)
    )
  }

  /**
   * Deterministically derives a child public key located at the given soft index.
   *
   * Follows section V.D from the BIP32-ED25519 spec.
   *
   * @param index the index of the key to derive
   * @return an extended public key
   */
  def deriveVerification(
    verificationKey: VerificationKeys.ExtendedEd25519,
    index:           Bip32Indexes.SoftIndex
  ): VerificationKeys.ExtendedEd25519 = {

    val z = ExtendedEd25519.hmac512WithKey(
      verificationKey.chainCode.data.toArray,
      Array(0x02.toByte) ++ verificationKey.ed25519.bytes.data.toArray ++ index.bytes.data
    )

    val zL = z.slice(0, 28)

    val zLMult8 = ByteBuffer
      .wrap(
        (8 * BigInt(1, zL.reverse.toArray)).toByteArray.reverse
          .padTo(32, 0: Byte)
      )
      .order(ByteOrder.LITTLE_ENDIAN)
      .array()
      .take(32)

    val scaledZL = new PointAccum
    scalarMultBase(zLMult8, scaledZL)

    val publicKeyPoint = new PointExt
    decodePointVar(verificationKey.ed25519.bytes.data.toArray, 0, negate = false, publicKeyPoint)

    pointAddVar(negate = false, publicKeyPoint, scaledZL)

    val nextPublicKeyBytes = new Array[Byte](PUBLIC_KEY_SIZE)
    encodePoint(scaledZL, nextPublicKeyBytes, 0)

    val nextPk = Bytes(nextPublicKeyBytes)

    val nextChainCode =
      ExtendedEd25519
        .hmac512WithKey(
          verificationKey.chainCode.data.toArray,
          Array(0x03.toByte) ++ verificationKey.ed25519.bytes.data.toArray ++ index.bytes.data
        )
        .slice(32, 64)

    VerificationKeys.ExtendedEd25519(
      VerificationKeys.Ed25519(Sized.strictUnsafe(nextPk)),
      Sized.strictUnsafe(nextChainCode)
    )
  }

  def generatePublicKey(secretKey: SecretKeys.ExtendedEd25519): VerificationKeys.ExtendedEd25519 = {
    val pk = new Array[Byte](PUBLIC_KEY_SIZE)
    scalarMultBaseEncoded(secretKey.leftKey.data.toArray, pk, 0)

    VerificationKeys.ExtendedEd25519(
      VerificationKeys.Ed25519(Sized.strictUnsafe(Bytes(pk))),
      secretKey.chainCode
    )
  }
}

object ExtendedEd25519 {
  val instance = new ExtendedEd25519
  instance.precompute()

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

  // Note: BigInt expects Big-Endian, but SLIP/BIP-ED25519 need Little-Endian
  def leftNumber(secretKey: SecretKeys.ExtendedEd25519): BigInt =
    BigInt(1, Bytes.toByteArray(secretKey.leftKey.data).reverse)

  def rightNumber(secretKey: SecretKeys.ExtendedEd25519): BigInt =
    BigInt(1, Bytes.toByteArray(secretKey.rightKey.data).reverse)

  /**
   * Validates that the given key is a valid derived key.
   * Keys are invalid if their left private keys are divisible by the ED-25519 Base Order N.
   * @param value the private key value
   * @return either an invalid error or the private key
   */
  def validate(value: SecretKeys.ExtendedEd25519): Either[InvalidDerivedKey, SecretKeys.ExtendedEd25519] =
    Either.cond(leftNumber(value) % edBaseN != 0, value, InvalidDerivedKey)

  /**
   * Instantiates an `ExtendedPrivateKeyEd25519` from entropy and a password.
   * @param entropy some random entropy
   * @param password an optional password for an extra layer of security
   * @return an `ExtendedPrivateKeyEd25519`
   */
  def fromEntropy(entropy: Entropy)(password: Password = ""): SecretKeys.ExtendedEd25519 = {

    val generatePrivateKey = (sizedSeed: Sized.Strict[Bytes, SecretKeys.ExtendedEd25519.Length]) => {
      val seed = sizedSeed.data.toArray

      // turn seed into a valid ExtendedPrivateKeyEd25519 per the SLIP-0023 spec
      seed(0) = (seed(0) & 0xf8).toByte
      seed(31) = ((seed(31) & 0x1f) | 0x40).toByte

      new SecretKeys.ExtendedEd25519(
        Sized.strictUnsafe(Bytes(seed.slice(0, 32).reverse)),
        Sized.strictUnsafe(Bytes(seed.slice(32, 64).reverse)),
        Sized.strictUnsafe(Bytes(seed.slice(64, 96).reverse))
      )
    }

    // first do a PBDKF2-HMAC-SHA512 per the SLIP2-0023 spec
    val seed: Sized.Strict[Bytes, SecretKeys.ExtendedEd25519.Length] =
      Sized.strictUnsafe(
        Bytes(
          Pbkdf2Sha512.generateKey(
            password.getBytes(StandardCharsets.UTF_8),
            entropy.value,
            96,
            4096
          )
        )
      )

    // turn seed into a valid ExtendedPrivateKeyEd25519 per the SLIP-0023 spec
    generatePrivateKey(seed)
  }

  private def hmac512WithKey(key: Array[Byte], data: Array[Byte]): Bytes = {
    val mac = new HMac(new SHA512Digest())
    mac.init(new KeyParameter(key))
    mac.update(data, 0, data.length)
    val out = new Array[Byte](64)
    mac.doFinal(out, 0)
    Bytes(out)
  }

  case object InvalidDerivedKey
  type InvalidDerivedKey = InvalidDerivedKey.type
}
