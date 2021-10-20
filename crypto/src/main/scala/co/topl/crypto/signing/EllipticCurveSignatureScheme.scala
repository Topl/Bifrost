package co.topl.crypto.signing

import co.topl.models.{Proof, SecretKey, VerificationKey}

import java.security.SecureRandom

/* Forked from https://github.com/input-output-hk/scrypto */

trait EllipticCurveSignatureScheme[SK <: SecretKey, VK <: VerificationKey, SIG <: Proof] {

  val SignatureLength: Int
  val KeyLength: Int

  def defaultRandom: Seed = {
    val random = SecureRandom.getInstance("SHA1PRNG", "SUN")
    random.nextBytes(Array(0: Byte)) // updating random seed per https://howtodoinjava.com/java8/secure-random-number-generation/
    Seed(random.generateSeed(128))
  }

  def createKeyPair(seed: Seed): (SK, VK)

  def createKeyPair: (SK, VK) = createKeyPair(defaultRandom)

  def sign(privateKey: SK, message: MessageToSign): SIG

  def verify(signature: SIG, message: MessageToSign, verifyKey: VK): Boolean
}