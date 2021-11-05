package co.topl.crypto.signing

import co.topl.crypto.Pbkdf2Sha512
import co.topl.crypto.mnemonic.Entropy
import co.topl.models.utility.HasLength.instances.bytesLength
import co.topl.models.utility.{Length, Sized}
import co.topl.models.{Bytes, Proof, SecretKey, VerificationKey}

import java.nio.charset.StandardCharsets

/* Forked from https://github.com/input-output-hk/scrypto */

abstract class EllipticCurveSignatureScheme[SK <: SecretKey, VK <: VerificationKey, SIG <: Proof, SeedLength <: Length](
  implicit seedLength: SeedLength
) {

  val SignatureLength: Int
  val KeyLength: Int

  def createKeyPair(entropy: Entropy, password: Option[Password]): (SK, VK) = {

    /**
     * do a PBDKF2-HMAC-SHA512 per the SLIP2-0023 Icarus spec
     */
    def entropyToSeed(entropy: Entropy, password: Option[Password]): Sized.Strict[Bytes, SeedLength] = {
      val kdf = new Pbkdf2Sha512()
      Sized.strictUnsafe(
        Bytes(
          kdf.generateKey(
            password.getOrElse("").getBytes(StandardCharsets.UTF_8),
            entropy.value,
            seedLength.value,
            4096
          )
        )
      )
    }

    createKeyPair((entropy, password), (entropyToSeed _).tupled)
  }

  def createKeyPair[T](
    t:      T,
    toSeed: T => Sized.Strict[Bytes, SeedLength]
  ): (SK, VK)

  def sign(privateKey: SK, message: Bytes): SIG

  def verify(signature: SIG, message: Bytes, verifyKey: VK): Boolean

  def getVerificationKey(privateKey: SK): VK
}
