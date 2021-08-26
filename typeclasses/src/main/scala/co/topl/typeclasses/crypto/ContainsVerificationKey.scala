package co.topl.typeclasses.crypto

trait ContainsVerificationKey[T, PublicKey] {
  def verificationKeyOf(privateKey: T): PublicKey
}
