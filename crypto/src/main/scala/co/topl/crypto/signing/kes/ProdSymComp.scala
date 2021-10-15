package co.topl.crypto.signing.kes

import co.topl.models.{Bytes, KeyData, Proofs}

import scala.math.BigInt

class ProdSymComp extends SumComposition {

  val symmetricLogL: Int = 9
  val exp_symmetricLogL: Int = exp(symmetricLogL)
  val maxSymmetricKeyTimeSteps: Int = exp_symmetricLogL * exp_symmetricLogL

  /**
   * Generate key in the MMM composition
   * @param seed input entropy for key generation
   * @return
   */

  def generateSymmetricProductKey(seed: Array[Byte], offset: Long): KeyData = {
    val r = prng(seed)
    val rp = prng(r._2)
    //super-scheme sum composition
    val L = generateSecretKey(r._1, symmetricLogL)
    //sub-scheme sum composition
    val Si = generateSecretKey(rp._1, symmetricLogL)
    val pki = generateVerificationKey(Si)
    val sig = sign(L, pki, 0)
    KeyData(L, Si, Bytes(sig), Bytes(pki), Bytes(rp._2), offset)
  }

  /**
   * Updates product keys to the specified time step
   * @param key input key
   * @param t_in input desired time step
   * @return  updated key
   */
  def updateSymmetricProductKey(key: KeyData, t_in: Int): KeyData = {
    val symmetricStepsL = exp_symmetricLogL
    val keyTime = getSymmetricProductKeyTimeStep(key)
    var L = key.superScheme
    var Si = key.subScheme
    var sig = key.subSchemeSignature.toArray
    var pki = key.subSchemePublicKey.toArray
    var seed = key.subSchemeSeed.toArray

    def treeTimeSteps(t: Int): (Int, Int) = {
      val tl = t / symmetricStepsL
      val ti = t % symmetricStepsL
      (tl, ti)
    }
    val (tl_in, ti_in) = treeTimeSteps(t_in)
    if (keyTime < t_in) {
      val tl = getKeyTime(L)
      var currentLeaf = tl
      if (tl < symmetricStepsL) while (tl_in > currentLeaf) {
        val r = prng(seed)
        seed = r._2
        currentLeaf += 1
        if (currentLeaf == tl_in) {
          Si = generateSecretKey(r._1, symmetricLogL)
          pki = generateVerificationKey(Si)
          L = updateKey(L, tl_in)
          sig = sign(L, pki, tl_in)
        } else {
          L = updateKey(L, currentLeaf)
        }
      }
      else {
        println("Error: max time steps reached")
      }
      val ti = getKeyTime(Si)
      if (ti_in < symmetricStepsL && ti_in > 0 && ti < ti_in) {
        Si = updateKey(Si, ti_in)
      }
    } else {
      println("Error: t less than given keyTime")
    }
    KeyData(L, Si, Bytes(sig), Bytes(pki), Bytes(seed), key.offset)
  }

  /**
   * Get the current time step of an MMM key
   * @param key MMM key to be inspected
   * @return Current time step of key
   */
  def getSymmetricProductKeyTimeStep(key: KeyData): Int = {
    val tl = getKeyTime(key.superScheme)
    val ti = getKeyTime(key.subScheme)
    exp_symmetricLogL * tl + ti
  }

  def signSymmetricProduct(key: KeyData, m: Array[Byte]): Array[Byte] = {
    val keyTime = BigInt(getSymmetricProductKeyTimeStep(key)).toByteArray
    val Si = key.subScheme
    val sigi = key.subSchemeSignature
    val pki = key.subSchemePublicKey
    val ti = getKeyTime(Si)
    val sigm = sign(Si, m ++ keyTime, ti)
    //KeyData(sigi, Bytes(sigm), pki, key.offset, Bytes(publicKey(key)))
    Array(0: Byte) //todo: fix
  }

  /**
   * Verify MMM signature
   * @param pk public key of the MMM secret key
   * @param m message corresponding to signature
   * @param sig signature to be verified
   * @return true if signature is valid false if otherwise
   */

  def verifySymmetricProductSignature(m: Array[Byte], sig: Array[Byte], t: Int): Boolean =
//    sig match {
//      case Proofs.Consensus.HdKes(sigi, sigm, pki, _, pkl) =>
//        val stepL = BigInt(sigi.slice(sigBytes + pkBytes, sigBytes + pkBytes + seedBytes).toArray).toInt
//        val stepSi = BigInt(sigm.slice(sigBytes + pkBytes, sigBytes + pkBytes + seedBytes).toArray).toInt
//        (
//          sumCompositionVerify(pkl.toArray, pki.toArray, sigi.toArray, stepL)
//            && sumCompositionVerify(pki.toArray, m ++ BigInt(t).toByteArray, sigm.toArray, stepSi)
//            && t == exp_symmetricLogL * stepL + stepSi
//          )
//    }
    true //todo: fix
}
