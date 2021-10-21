package co.topl.crypto.signing.kes

import co.topl.models.utility.KesBinaryTree.{Empty, MerkleNode, SigningLeaf}

import scala.annotation.tailrec

class ProductComposition extends KesEd25519Blake2b256 {

  protected val sumComposition = new SumComposition

  override type SIG = (sumComposition.SIG, sumComposition.SIG, Array[Byte])
  override type VK = (Array[Byte], Int)
  override type SK = (sumComposition.SK, sumComposition.SK, Array[Byte], sumComposition.SIG)

  /**
   * Get the current time step of an MMM key
   * @param key MMM key to be inspected
   * @return Current time step of key
   */
  private[signing] def getKeyTime(key: SK): Int = {
    val numSubSteps = exp(sumComposition.getTreeHeight(key._2))
    val tSup = sumComposition.getKeyTime(key._1)
    val tSub = sumComposition.getKeyTime(key._2)
    (tSup * numSubSteps) + tSub
  }

  /**
   * @param key
   * @return
   */
  private[signing] def generateVerificationKey(key: SK): VK = key._1 match {
    case node: MerkleNode  => (witness(node), getKeyTime(key))
    case leaf: SigningLeaf => (witness(leaf), 0)
    case Empty             => (Array.fill(hashBytes)(0: Byte), 0)
  }

  /**
   * Generate key in the MMM composition
   * @param seed input entropy for key generation
   * @return
   */
  private[signing] def generateSecretKey(seed: Array[Byte], heightSup: Int, heightSub: Int): SK = {
    val rSuper = prng(seed)
    val rSub = prng(rSuper._2)
    val superScheme = sumComposition.generateSecretKey(rSuper._1, heightSup)
    val subScheme = sumComposition.generateSecretKey(rSub._1, heightSub)
    val kesVkSub: sumComposition.VK = sumComposition.generateVerificationKey(subScheme)
    val kesSigSup: sumComposition.SIG = sumComposition.sign(superScheme, kesVkSub._1)

    (superScheme, subScheme, rSub._2, kesSigSup)
  }

  /**
   * Updates product keys to the specified time step
   * @param key input key
   * @param t_in input desired time step
   * @return  updated key
   */
  private[signing] def updateKey(key: SK, step: Int): SK = {
    val keyTime = getKeyTime(key)
    val keyTimeSup = sumComposition.getKeyTime(key._1)
    val heightSup = sumComposition.getTreeHeight(key._1)
    val heightSub = sumComposition.getTreeHeight(key._2)
    val totalSteps = exp(heightSup + heightSub)
    val totalStepsSub = exp(heightSub)
    val newKeyTimeSup = step / totalStepsSub
    val newKeyTimeSub = step % totalStepsSub

    @tailrec
    def getSeed(seeds: (Array[Byte], Array[Byte]), iter: Int): (Array[Byte], Array[Byte]) = {
      if (iter < newKeyTimeSup) getSeed(prng(seeds._2), iter + 1)
      else seeds
    }

    if (step == 0) key
    else if (step > keyTime && step < totalSteps) {
      if (keyTimeSup < newKeyTimeSup) {
        val (s1, s2) = getSeed((Array(), key._3), keyTimeSup)
        val superScheme = sumComposition.evolveKey(key._1, newKeyTimeSup)
        val newSubScheme = sumComposition.generateSecretKey(s1, heightSub)
        val kesVkSub = sumComposition.generateVerificationKey(newSubScheme)
        val kesSigSuper = sumComposition.sign(superScheme, kesVkSub._1)
        val updatedSubScheme = sumComposition.evolveKey(newSubScheme, newKeyTimeSub)
        (superScheme, updatedSubScheme, s2, kesSigSuper)
      } else {
        val subScheme = sumComposition.updateKey(key._2, newKeyTimeSub)
        (key._1, subScheme, key._3, key._4)
      }
    } else {
      throw new Error(
        s"Update error - Max steps: $totalSteps, current step: $keyTime, requested increase: $step"
      )
    }
  }

  /**
   * @param key
   * @param m
   * @return
   */
  private[signing] def sign(key: SK, m: Array[Byte]): SIG =
    (key._4, sumComposition.sign(key._2, m), sumComposition.generateVerificationKey(key._2)._1)

  /**
   * Verify MMM signature
   * @param pk public key of the MMM secret key
   * @param m message corresponding to signature
   * @param sig signature to be verified
   * @return true if signature is valid false if otherwise
   */
  private[signing] def verify(kesSig: SIG, m: Array[Byte], kesVk: VK): Boolean = {
    val totalStepsSub = exp(kesSig._2._3.length)
    val keyTimeSup = kesVk._2 / totalStepsSub
    val keyTimeSub = kesVk._2 % totalStepsSub

    val verifySup = sumComposition.verify(kesSig._1, kesSig._3, (kesVk._1, keyTimeSup))
    val verifySub = sumComposition.verify(kesSig._2, m, (kesSig._3, keyTimeSub))

    verifySup && verifySub
  }
}
