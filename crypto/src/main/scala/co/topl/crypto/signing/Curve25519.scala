package co.topl.crypto.signing

import co.topl.models.utility.HasLength.instances.bytesLength
import co.topl.models.utility.{Lengths, Sized}
import co.topl.models.{Bytes, Proofs, SecretKeys, VerificationKeys}
import org.whispersystems.curve25519.OpportunisticCurve25519Provider

import java.lang.reflect.Constructor

/* Forked from https://github.com/input-output-hk/scrypto */
class Curve25519
    extends EllipticCurveSignatureScheme[
      SecretKeys.Curve25519,
      VerificationKeys.Curve25519,
      Proofs.Signature.Curve25519,
      SecretKeys.Curve25519.Length
    ] {

  override val SignatureLength: Int = 64
  override val KeyLength: Int = 32

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

  override protected def createKeyPair(
    seed: Sized.Strict[Bytes, SecretKeys.Curve25519.Length]
  ): (SecretKeys.Curve25519, VerificationKeys.Curve25519) = {
    val privateKey = SecretKeys.Curve25519(Sized.strictUnsafe(Bytes(provider.generatePrivateKey(seed.data.toArray))))
    val publicKey = VerificationKeys.Curve25519(
      Sized.strictUnsafe(Bytes(provider.generatePublicKey(privateKey.bytes.data.toArray)))
    )
    privateKey -> publicKey
  }

  override def sign(
    privateKey: SecretKeys.Curve25519,
    message:    Bytes
  ): Proofs.Signature.Curve25519 =
    Proofs.Signature.Curve25519(
      Sized.strictUnsafe(
        Bytes(
          provider.calculateSignature(
            provider.getRandom(SignatureLength),
            privateKey.bytes.data.toArray,
            message.toArray
          )
        )
      )
    )

  override def verify(
    signature: Proofs.Signature.Curve25519,
    message:   Bytes,
    publicKey: VerificationKeys.Curve25519
  ): Boolean =
    signature.bytes.data.length == SignatureLength &&
    publicKey.bytes.data.length == KeyLength &&
    provider.verifySignature(
      publicKey.bytes.data.toArray,
      message.toArray,
      signature.bytes.data.toArray
    )

  override def getVerificationKey(secretKey: SecretKeys.Curve25519): VerificationKeys.Curve25519 =
    VerificationKeys.Curve25519(Sized.strictUnsafe(Bytes(provider.generatePublicKey(secretKey.bytes.data.toArray))))
}

object Curve25519 {
  val instance = new Curve25519
}
