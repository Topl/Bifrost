package co.topl.crypto.signing.kes

import co.topl.models.{Bytes, KeyData, Proofs}

import scala.math.BigInt

class ProdAsymComp extends SumComposition {

  val asymmetricLogL: Int = 7

  def generateAsymmetricProductKey(seed: Array[Byte], offset: Long): KeyData = {
    val r = PRNG(seed)
    val rp = PRNG(r._2)
    //super-scheme sum composition
    val L = sumCompositionGenerateKey(r._1, asymmetricLogL)
    //sub-scheme sum composition
    val Si = sumCompositionGenerateKey(rp._1, 0)
    val pki = sumCompositionGetPublicKey(Si)
    val sig = sumCompositionSign(L, pki, 0)
    KeyData(L, Si, Bytes(sig), Bytes(pki), Bytes(rp._2), offset)
  }

  /**
   * Signature in the MMM composition
   * @param key signing secret key
   * @param m message to be signed
   * @return signature of m
   */
  def signAsymmetricProduct(key: KeyData, m: Array[Byte]): Array[Byte] = {
    val keyTime = BigInt(getAsymmetricProductKeyTimeStep(key)).toByteArray
    val Si = key.subScheme
    val sigi = key.subSchemeSignature
    val pki = key.subSchemePublicKey
    val ti = getSumCompositionKeyTimeStep(Si)
    val sigm = sumCompositionSign(Si, m ++ keyTime, ti)
    //KeyData(sigi, Bytes(sigm), pki, key.offset, Bytes(publicKey(key)))
    Array(0: Byte) //todo: fix
  }

  def updateAsymmetricProductKey(key: KeyData, t_in: Int): KeyData = {
    val keyTime = getAsymmetricProductKeyTimeStep(key)
    var L = key.superScheme
    var Si = key.subScheme
    var sig = key.subSchemeSignature.toArray
    var pki = key.subSchemePublicKey.toArray
    var seed = key.subSchemeSeed.toArray
    def timeStepModLog2(t: Int): (Int, Int) =
      if (t == 0) {
        (0, 0)
      } else {
        var e = 2
        var n = 1
        var found = false
        while (!found) {
          val next_power = exp(n + 1)
          if (t + 1 > next_power - 1) {
            n += 1
            e = next_power
          } else {
            found = true
          }
        }
        val tl = n
        val ti = (t + 1) % e
        (tl, ti)
      }
    val (tl_in, ti_in) = timeStepModLog2(t_in)
    if (keyTime < t_in) {
      val Tl = exp(L.height)
      val tl = getSumCompositionKeyTimeStep(L)
      var currentLeaf = tl
      if (tl < Tl) while (tl_in > currentLeaf) {
        val r = PRNG(seed)
        seed = r._2
        currentLeaf += 1
        if (currentLeaf == tl_in) {
          Si = sumCompositionGenerateKey(r._1, tl_in)
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
      val Ti = exp(Si.height)
      val ti = getSumCompositionKeyTimeStep(Si)
      if (ti_in < Ti && ti_in > 0 && ti < ti_in) {
        Si = sumUpdateFast(Si, ti_in)
      }
    } else {
      println("Error: t less than given keyTime")
    }
    KeyData(L, Si, Bytes(sig), Bytes(pki), Bytes(seed), key.offset)
  }

  def getAsymmetricProductKeyTimeStep(key: KeyData): Int = {
    val tl = getSumCompositionKeyTimeStep(key.superScheme)
    val ti = getSumCompositionKeyTimeStep(key.subScheme)
    exp(tl) - 1 + ti
  }

  /**
   * Verify MMM signature
   * @param pk public key of the MMM secret key
   * @param m message corresponding to signature
   * @param sig signature to be verified
   * @return true if signature is valid false if otherwise
   */

  def verifyAsymmetricProductSignature(m: Array[Byte], sig: Proofs.Signature.KesAsymmetricProduct, t: Int): Boolean =
    true //todo: fix
//    sig match {
//      case Proofs.Signature.HdKes(sigi, sigm, pki, _, pkl) =>
//        val stepL = BigInt(sigi.slice(sigBytes + pkBytes, sigBytes + pkBytes + seedBytes).toArray).toInt
//        val stepSi = BigInt(sigm.slice(sigBytes + pkBytes, sigBytes + pkBytes + seedBytes).toArray).toInt
//        (sumCompositionVerify(pkl.toArray, pki.toArray, sigi.toArray, stepL)
//        && sumCompositionVerify(pki.toArray, m ++ BigInt(t).toByteArray, sigm.toArray, stepSi)
//        && t == exp(stepL) - 1 + stepSi)
//    }

}
