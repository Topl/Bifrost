package co.topl.crypto.signing

import co.topl.crypto.mnemonic.Entropy
import co.topl.models.{Bytes, Proof, SecretKey, VerificationKey}

/* Forked from https://github.com/input-output-hk/scrypto */

trait EllipticCurveSignatureScheme[SK <: SecretKey, VK <: VerificationKey, SIG <: Proof] {

  val SignatureLength: Int
  val KeyLength: Int

  def createKeyPair(seed: Bytes): (SK, VK)

  def sign(privateKey: SK, message: Bytes): SIG

  def verify(signature: SIG, message: Bytes, verifyKey: VK): Boolean

  def getVerificationKey(privateKey: SK): VK
}
