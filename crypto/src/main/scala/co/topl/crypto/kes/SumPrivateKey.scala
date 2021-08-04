package co.topl.crypto.kes

case class SumPrivateKey(L:Tree[Array[Byte]]) {

  def update(kes:KeyEvolvingSignatureScheme, t:Int): SumPrivateKey = {
    SumPrivateKey(kes.sumUpdateFast(L,t))
  }

  def sign(kes:KeyEvolvingSignatureScheme, m:Array[Byte]): Array[Byte] = {
    kes.sumSign(L,m,kes.sumGetKeyTimeStep(L))
  }

  def getVerificationKey(kes:KeyEvolvingSignatureScheme): Array[Byte] = {
    kes.sumGetPublicKey(L)
  }

  def timeStep(kes:KeyEvolvingSignatureScheme):Int = {
    kes.sumGetKeyTimeStep(L)
  }
}

object SumPrivateKey {
  def newFromSeed(kes:KeyEvolvingSignatureScheme, seed:Array[Byte], logl:Int):SumPrivateKey = {
    SumPrivateKey(kes.sumGenerateKey(seed,logl))
  }
}
