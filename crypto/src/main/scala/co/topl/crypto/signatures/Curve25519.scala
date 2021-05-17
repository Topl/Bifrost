package co.topl.crypto.signatures

import cats.implicits._
import co.topl.crypto.hash.Sha256
import org.whispersystems.curve25519.OpportunisticCurve25519Provider

import java.lang.reflect.Constructor

/* Forked from https://github.com/input-output-hk/scrypto */

object Curve25519 extends EllipticCurveSignatureScheme {

  val SignatureLength25519 = 64
  val KeyLength25519 = 32

  override val SignatureLength = SignatureLength25519
  override val KeyLength = KeyLength25519

  /* todo: dirty hack, switch to logic as described in WhisperSystem's Curve25519 tutorial when
              it would be possible to pass a random seed from outside, see
              https://github.com/WhisperSystems/curve25519-java/pull/7
   */
  private val provider: OpportunisticCurve25519Provider = {
    val constructor = classOf[OpportunisticCurve25519Provider].getDeclaredConstructors.head
      .asInstanceOf[Constructor[OpportunisticCurve25519Provider]]
    constructor.setAccessible(true)
    constructor.newInstance()
  }

  override def createKeyPair(seed: Array[Byte]): CreateKeyPairResult =
    (for {
      hashedSeed <- Sha256.hash(seed)
      privateKey = PrivateKey(provider.generatePrivateKey(hashedSeed.value))
      publicKey = PublicKey(provider.generatePublicKey(privateKey.value))
    } yield privateKey -> publicKey) leftMap PrivateKeyHashFailure

  override def sign(privateKey: PrivateKey, message: MessageToSign): Signature = {
    require(privateKey.value.length == KeyLength)
    Signature(provider.calculateSignature(provider.getRandom(SignatureLength), privateKey.value, message))
  }

  override def verify(signature: Signature, message: MessageToSign, publicKey: PublicKey): Boolean =
    signature.value.length == SignatureLength &&
    publicKey.value.length == KeyLength &&
    provider.verifySignature(publicKey.value, message, signature.value)

  override def createSharedSecret(privateKey: PrivateKey, publicKey: PublicKey): SharedSecret =
    SharedSecret(provider.calculateAgreement(privateKey.value, publicKey.value))

}
