package co.topl.crypto.signing.kes

import co.topl.crypto.signing.eddsa.Ed25519
import co.topl.models.{Bytes, KeyData, Proofs}

import scala.math.BigInt

class ProdSymComp extends SumComposition {

  /**
   * Generate key in the MMM composition
   * @param seed input entropy for key generation
   * @return
   */

  def generateSymmetricProductKey(seed: Array[Byte], offset: Long): KeyData = {
    val r = PRNG(seed)
    val rp = PRNG(r._2)
    //super-scheme sum composition
    val L = sumCompositionGenerateKey(r._1, symmetricLogL)
    //sub-scheme sum composition
    val Si = sumCompositionGenerateKey(rp._1, symmetricLogL)
    val pki = sumCompositionGetPublicKey(Si)
    val sig = sumCompositionSign(L, pki, 0)
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
      val tl = getSumCompositionKeyTimeStep(L)
      var currentLeaf = tl
      if (tl < symmetricStepsL) while (tl_in > currentLeaf) {
        val r = PRNG(seed)
        seed = r._2
        currentLeaf += 1
        if (currentLeaf == tl_in) {
          Si = sumCompositionGenerateKey(r._1, symmetricLogL)
          pki = sumCompositionGetPublicKey(Si)
          L = sumCompositionUpdate(L, tl_in)
          sig = sumCompositionSign(L, pki, tl_in)
        } else {
          L = sumCompositionUpdate(L, currentLeaf)
        }
      }
      else {
        println("Error: max time steps reached")
      }
      val ti = getSumCompositionKeyTimeStep(Si)
      if (ti_in < symmetricStepsL && ti_in > 0 && ti < ti_in) {
        Si = sumUpdateFast(Si, ti_in)
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
    val tl = getSumCompositionKeyTimeStep(key.superScheme)
    val ti = getSumCompositionKeyTimeStep(key.subScheme)
    exp_symmetricLogL * tl + ti
  }

  def signSymmetricProduct(key: KeyData, m: Array[Byte]): Array[Byte] = {
    val keyTime = BigInt(getSymmetricProductKeyTimeStep(key)).toByteArray
    val Si = key.subScheme
    val sigi = key.subSchemeSignature
    val pki = key.subSchemePublicKey
    val ti = getSumCompositionKeyTimeStep(Si)
    val sigm = sumCompositionSign(Si, m ++ keyTime, ti)
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

  def verifySymmetricProductSignature(m: Array[Byte], sig: Proofs.Signature.KesSymmetricProduct, t: Int): Boolean =
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
