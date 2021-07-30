package co.topl.crypto.kes

import com.google.common.primitives.Longs

/**
  * AMS 2021:
  * This is a private key of the MMM construction with a specified time step offset
  * the offset is constrained by the public key and signatures include the offset,
  * The age of keys may be enforced in validation where the offset can be compared
  * to the time step of the signature since signatures include the offset
  */

case class ProductPrivateKey(
                         var L:Tree[Array[Byte]],
                         var Si:Tree[Array[Byte]],
                         var sig:Array[Byte],
                         var pki:Array[Byte],
                         var rp:Array[Byte],
                         var offset:Long
                       ) {

  def update(kes:KeyEvolvingSignature, t:Long): Unit = {
    val updatedKey = kes.updateKeyFast((L,Si,sig,pki,rp),(t-offset).toInt)
    L = updatedKey._1
    Si = updatedKey._2
    sig = updatedKey._3
    pki = updatedKey._4
    rp = updatedKey._5
  }

  def sign(kes:KeyEvolvingSignature, m:Array[Byte]): (Array[Byte],Array[Byte],Array[Byte],Long,Array[Byte]) = {
    val out = kes.sign((L,Si,sig,pki,rp),m)
    (out._1,out._2,out._3,offset,kes.publicKey((L,Si,sig,pki,rp)))
  }

  def getPublic(kes:KeyEvolvingSignature, fch:Fch):Array[Byte] = {
    val pk_kes = kes.publicKey((L,Si,sig,pki,rp))
    fch.hash(Longs.toByteArray(offset)++pk_kes)
  }

  def time(kes:KeyEvolvingSignature):Long = {
    kes.getKeyTimeStep((L,Si,sig,pki,rp)) + offset
  }

}

object ProductPrivateKey {
  def newFromSeed(kes:KeyEvolvingSignature, seed:Array[Byte], t:Long):ProductPrivateKey = {
    val keyData = kes.generateKey(seed)
    ProductPrivateKey(keyData._1, keyData._2, keyData._3, keyData._4, keyData._5, t)
  }
}
