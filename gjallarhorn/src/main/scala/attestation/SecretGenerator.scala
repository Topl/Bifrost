package attestation

/**
 * SecretGenerator is a type-class that describes a class ability to generate a secret
 *
 * @tparam S a keyfile class that can be used to generate new secrets
 */
sealed trait SecretGenerator[S <: Secret] {
  def generateSecret(seed: Array[Byte]): (S, S#PK)
}

object SecretGenerator {
  def apply[S <: Secret](implicit ev: SecretGenerator[S]): SecretGenerator[S] = ev

  def instance[S <: Secret](f:        Array[Byte] => (S, S#PK)): SecretGenerator[S] = new SecretGenerator[S] {
    override def generateSecret(seed: Array[Byte]): (S, S#PK) = f(seed)
  }
}
