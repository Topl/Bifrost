package co.topl.crypto.signing

import co.topl.crypto.generation.{EntropyToSeed, Pbkdf2Sha512}
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

  def createKeyPair(seed: Sized.Strict[Bytes, SeedLength]): (SK, VK)

  def sign(privateKey: SK, message: Bytes): SIG

  def verify(signature: SIG, message: Bytes, verifyKey: VK): Boolean

  def getVerificationKey(privateKey: SK): VK
}
