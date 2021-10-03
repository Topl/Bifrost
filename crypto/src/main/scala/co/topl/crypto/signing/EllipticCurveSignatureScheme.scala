package co.topl.crypto.signing

import co.topl.models.{Proof, SecretKey, VerificationKey}

/* Forked from https://github.com/input-output-hk/scrypto */

trait EllipticCurveSignatureScheme[SK <: SecretKey, VK <: VerificationKey, SIG <: Proof] {

  val SignatureLength: Int
  val KeyLength: Int

  def createKeyPair(seed: Seed): (SK, VK)

  def createKeyPair: (SK, VK)

  def sign(privateKey: SK, message: MessageToSign): SIG

  def verify(signature: SIG, message: MessageToSign, verifyKey: VK): Boolean
}