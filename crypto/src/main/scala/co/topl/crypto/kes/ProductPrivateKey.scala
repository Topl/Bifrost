package co.topl.crypto.kes

import com.google.common.primitives.Longs

/**
  * AMS 2021:
  * This is a private key of the MMM construction the asymmetric product composition with a specified time step offset
  * the offset is constrained by the public key and signatures include the offset,
  * The age of keys may be enforced in validation where the offset can be compared
  * to the time step of the signature since signatures include the offset
  */

case class ProductPrivateKey(
                              L:Tree[Array[Byte]],
                              Si:Tree[Array[Byte]],
                              sig:Array[Byte],
                              pki:Array[Byte],
                              rp:Array[Byte],
                              offset:Long
                       ) {

  def update(kes:KeyEvolvingSignatureScheme, t:Long): ProductPrivateKey = {
    val updatedKey = kes.updateKeyFast((L,Si,sig,pki,rp),(t-offset).toInt)
    ProductPrivateKey(updatedKey._1, updatedKey._2, updatedKey._3, updatedKey._4, updatedKey._5, offset)
  }

  def sign(kes:KeyEvolvingSignatureScheme, m:Array[Byte]): ProductSignature = {
    val out = kes.sign((L,Si,sig,pki,rp),m)
    ProductSignature(out._1,out._2,out._3,offset,kes.publicKey((L,Si,sig,pki,rp)))
  }

  def getVerificationKey(kes:KeyEvolvingSignatureScheme, fch:Fch):Array[Byte] = {
    val pk_kes = kes.publicKey((L,Si,sig,pki,rp))
    fch.hash(Longs.toByteArray(offset) ++ pk_kes)
  }

  def timeStep(kes:KeyEvolvingSignatureScheme):Long = {
    kes.getKeyTimeStep((L,Si,sig,pki,rp)) + offset
  }
}

object ProductPrivateKey {
  def newFromSeed(kes:KeyEvolvingSignatureScheme, seed:Array[Byte], t:Long):ProductPrivateKey = {
    val keyData = kes.generateKey(seed)
    ProductPrivateKey(keyData._1, keyData._2, keyData._3, keyData._4, keyData._5, t)
  }
}
