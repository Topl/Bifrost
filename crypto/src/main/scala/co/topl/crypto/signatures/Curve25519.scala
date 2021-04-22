package co.topl.crypto.signatures

import co.topl.crypto.hash.Hash
import org.whispersystems.curve25519.OpportunisticCurve25519Provider

import java.lang.reflect.Constructor
import scala.util.{Failure, Try}

/* Forked from https://github.com/input-output-hk/scrypto */

object Curve25519 extends EllipticCurveSignatureScheme {

  // use Sha256 hashing
  import co.topl.crypto.hash.Sha256._

  val SignatureLength25519 = 64
  val KeyLength25519 = 32

  override val SignatureLength = SignatureLength25519
  override val KeyLength = KeyLength25519

  /* todo: dirty hack, switch to logic as described in WhisperSystem's Curve25519 tutorial when
              it would be possible to pass a random seed from outside, see
              https://github.com/WhisperSystems/curve25519-java/pull/7
  */
  private val provider: OpportunisticCurve25519Provider = {
    val constructor = classOf[OpportunisticCurve25519Provider]
      .getDeclaredConstructors
      .head
      .asInstanceOf[Constructor[OpportunisticCurve25519Provider]]
    constructor.setAccessible(true)
    constructor.newInstance()
  }

  override def createKeyPair(seed: Array[Byte]): (PrivateKey, PublicKey) = {
    val hashedSeed = Hash(seed)
    val privateKey = PrivateKey @@ provider.generatePrivateKey(hashedSeed.bytes)
    privateKey -> PublicKey @@ provider.generatePublicKey(privateKey)
  }

  override def sign(privateKey: PrivateKey, message: MessageToSign): Signature = {
    require(privateKey.length == KeyLength)
    Signature @@ provider.calculateSignature(provider.getRandom(SignatureLength), privateKey, message)
  }

  override def verify(signature: Signature, message: MessageToSign, publicKey: PublicKey): Boolean = Try {
    require(signature.length == SignatureLength)
    require(publicKey.length == KeyLength)
    provider.verifySignature(publicKey, message, signature)
  }.recoverWith { case e =>
    // TODO: Jing - remove this log
    // log.debug("Error while message signature verification", e)
    Failure(e)
  }.getOrElse(false)

  override def createSharedSecret(privateKey: PrivateKey, publicKey: PublicKey): SharedSecret = {
    SharedSecret @@ provider.calculateAgreement(privateKey, publicKey)
  }
}