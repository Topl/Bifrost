package co.topl.crypto.signing

import co.topl.crypto.generation.Pbkdf2Sha512
import co.topl.crypto.generation.mnemonic.Entropy
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

  def createKeyPair(entropy: Entropy, password: Option[Password])(implicit
    entropyToSeed:           EntropyToSeed[SeedLength] = EntropyToSeed.instances.pbkdf2Sha512[SeedLength]
  ): (SK, VK) = {
    val seed = entropyToSeed.toSeed(entropy, password)
    createKeyPair(seed)
  }

  protected def createKeyPair(seed: Sized.Strict[Bytes, SeedLength]): (SK, VK)

  def sign(privateKey: SK, message: Bytes): SIG

  def verify(signature: SIG, message: Bytes, verifyKey: VK): Boolean

  def getVerificationKey(privateKey: SK): VK
}

trait EntropyToSeed[SeedLength <: Length] {
  def toSeed(entropy: Entropy, password: Option[Password]): Sized.Strict[Bytes, SeedLength]
}

object EntropyToSeed {

  trait Instances {

    implicit def pbkdf2Sha512[SeedLength <: Length](implicit seedLength: SeedLength): EntropyToSeed[SeedLength] =
      (entropy: Entropy, password: Option[Password]) => {
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

  }

  object instances extends Instances
}
