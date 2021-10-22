package co.topl.crypto.signing

import co.topl.models.{Bytes, Proof, SecretKey, VerificationKey}

trait KeyEvolvingSignatureScheme[SK <: SecretKey, VK <: VerificationKey, SIG <: Proof, H] {

  def createKeyPair(seed: Bytes, height: H, offset: Long): (SK, VK)

  def sign(privateKey: SK, message: Bytes): SIG

  def verify(signature: SIG, message: Bytes, verifyKey: VK): Boolean

  def update(privateKey: SK, steps: Int): SK

  def getCurrentStep(privateKay: SK): Int

  def getMaxStep(privateKey: SK): Int

  def getVerificationKey(privateKey: SK): VK
}
