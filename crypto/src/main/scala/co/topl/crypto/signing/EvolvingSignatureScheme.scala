package co.topl.crypto.signing

trait EvolvingSignatureScheme[SK, VK, I] {
  def deriveSecret(secretKey:             SK, index: I): SK
  def deriveVerification(verificationKey: VK, index: I): VK
}
