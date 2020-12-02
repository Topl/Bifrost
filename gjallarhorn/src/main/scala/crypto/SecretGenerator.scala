package crypto

sealed trait SecretGenerator[S <: Secret] {
  def generateSecret (seed: Array[Byte]): (S, S#PK)
}

object SecretGenerator {
  def apply[S <: Secret](implicit ev: SecretGenerator[S]): SecretGenerator[S] = ev
  def instance[S <: Secret](f: Array[Byte] => (S, S#PK)): SecretGenerator[S] = new SecretGenerator[S] {
    override def generateSecret (seed: Array[Byte]): (S, S#PK) = f(seed)
  }
}
