package co.topl.crypto.signing

import co.topl.models.{Proof, SecretKey, VerificationKey}

import java.security.SecureRandom

trait KeyEvolvingSignatureScheme[SK <: SecretKey, VK <: VerificationKey, SIG <: Proof, H] {

  val defaultHeight: H
  val defaultOffset: Int

  def defaultRandom: Seed = {
    val random = SecureRandom.getInstance("SHA1PRNG", "SUN")

    // updating random seed per https://howtodoinjava.com/java8/secure-random-number-generation/
    random.nextBytes(Array(0: Byte))
    Seed(random.generateSeed(128))
  }

  def createKeyPair: (SK, VK) = createKeyPair(defaultRandom, defaultHeight, defaultOffset)

  def createKeyPair(seed: Seed, height: H, offset: Int): (SK, VK)

  def sign(privateKey: SK, message: MessageToSign): SIG

  def verify(signature: SIG, message: MessageToSign, verifyKey: VK): Boolean

  def update(privateKey: SK, steps: Int): SK

  def getKeyTime(privateKay: SK): Int

  def getVerificationKey(privateKey: SK): VK
}
