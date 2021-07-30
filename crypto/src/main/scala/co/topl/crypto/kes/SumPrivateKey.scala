package co.topl.crypto.kes

import com.google.common.primitives.Longs

/**
  * AMS 2021:
  * This is a private key of the MMM construction with a specified time step offset
  * the offset is constrained by the public key and signatures include the offset,
  * The age of keys may be enforced in validation where the offset can be compared
  * to the time step of the signature since signatures include the offset
  */

case class SumPrivateKey(var L:Tree[Array[Byte]], var offset:Long) {

  def update(kes:KeyEvolvingSignature, t:Long): Unit = {
    val updatedKey = kes.sumUpdateFast(L,(t-offset).toInt)
    L = updatedKey
  }

  def sign(kes:KeyEvolvingSignature, m:Array[Byte]): (Array[Byte],Long,Array[Byte]) = {
    val out = kes.sumSign(L,m,kes.sumGetKeyTimeStep(L))
    (out,offset,kes.sumGetPublicKey(L))
  }

  def getPublic(kes:KeyEvolvingSignature, fch:Fch):Array[Byte] = {
    val pk_kes = kes.sumGetPublicKey(L)
    fch.hash(Longs.toByteArray(offset)++pk_kes)
  }

  def time(kes:KeyEvolvingSignature):Long = {
    kes.sumGetKeyTimeStep(L) + offset
  }
}

object SumPrivateKey {
  def newFromSeed(kes:KeyEvolvingSignature, seed:Array[Byte], t:Long, logl:Int):SumPrivateKey = {
    val keyData = kes.sumGenerateKey(seed,logl)
    SumPrivateKey(keyData,t)
  }
}
