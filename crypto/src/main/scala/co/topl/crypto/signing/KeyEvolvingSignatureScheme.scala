package co.topl.crypto.signing

import co.topl.models.{Proof, SecretKey, VerificationKey}

trait KeyEvolvingSignatureScheme[SK <: SecretKey, VK <: VerificationKey, SIG <: Proof, H] {

  def createKeyPair(seed: Seed, height: H, offset: Int): (SK, VK)

  def sign(privateKey: SK, message: MessageToSign): SIG

  def verify(signature: SIG, message: MessageToSign, verifyKey: VK): Boolean

  def update(privateKey: SK, steps: Int): SK

  def getCurrentStep(privateKay: SK): Int

  def getMaxStep(privateKey: SK): Int

  def getVerificationKey(privateKey: SK): VK
}
