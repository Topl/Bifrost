package co.topl.stakeholder.primitives

import com.google.common.primitives.Ints

/**
 * AMS 2020:
 * Forging Key is a private key of the MMM construction with a specified time step offset
 * the offset is constrained by the public key and signatures include the offset,
 * The VRF private key and this KES private key are required to test and forge respectively,
 * The age of keys may be enforced in header validation where the offset can be compared
 * to the slot of the header since signatures include the offset
 */

class ForgingKey {
  var L: Tree[Array[Byte]] = Leaf(Array())
  var Si: Tree[Array[Byte]] = Leaf(Array())
  var sig: Array[Byte] = Array()
  var pki: Array[Byte] = Array()
  var rp: Array[Byte] = Array()
  var offset: Int = 0
  val fch = new Fch

  def update(kes: Kes, t: Int): Unit = {
    val updatedKey = kes.updateKey((L, Si, sig, pki, rp), t - offset)
    L = updatedKey._1
    Si = updatedKey._2
    sig = updatedKey._3
    pki = updatedKey._4
    rp = updatedKey._5
  }

  def update_fast(kes: Kes, t: Int): Unit = {
    val updatedKey = kes.updateKeyFast((L, Si, sig, pki, rp), t - offset)
    L = updatedKey._1
    Si = updatedKey._2
    sig = updatedKey._3
    pki = updatedKey._4
    rp = updatedKey._5
  }

  def sign(kes: Kes, m: Array[Byte]): (Array[Byte], Array[Byte], Array[Byte], Int, Array[Byte]) = {
    val out = kes.sign((L, Si, sig, pki, rp), m)
    (out._1, out._2, out._3, offset, kes.publicKey((L, Si, sig, pki, rp)))
  }

  def getPublic(kes: Kes): Array[Byte] = {
    val pk_kes = kes.publicKey((L, Si, sig, pki, rp))
    fch.hash(Ints.toByteArray(offset) ++ pk_kes)
  }

  def time(kes: Kes): Int =
    kes.getKeyTimeStep((L, Si, sig, pki, rp)) + offset
}

object ForgingKey {

  def apply(kes: Kes, seed: Array[Byte], t: Int): ForgingKey = {
    val keyData = kes.generateKey(seed)
    val newKey = new ForgingKey
    newKey.L = keyData._1
    newKey.Si = keyData._2
    newKey.sig = keyData._3
    newKey.pki = keyData._4
    newKey.rp = keyData._5
    newKey.offset = t
    newKey
  }

  def apply(
    L:      Tree[Array[Byte]],
    Si:     Tree[Array[Byte]],
    sig:    Array[Byte],
    pki:    Array[Byte],
    rp:     Array[Byte],
    offset: Int
  ): ForgingKey = {
    val newKey = new ForgingKey
    newKey.L = L
    newKey.Si = Si
    newKey.sig = sig
    newKey.pki = pki
    newKey.rp = rp
    newKey.offset = offset
    newKey
  }
}
