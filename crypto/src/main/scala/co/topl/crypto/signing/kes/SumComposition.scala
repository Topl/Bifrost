package co.topl.crypto.signing.kes

import scala.annotation.tailrec

/**
 * AMS 2021:
 * Implementation of the MMM construction:
 * Malkin, T., Micciancio, D. and Miner, S. (2002) ‘Efficient generic
 * forward-secure signatures with an unbounded number of time
 * periods’, Advances in Cryptology Eurocrypt ’02, LNCS 2332,
 * Springer, pp.400–417.
 *
 * Provides forward secure signatures that cannot be reforged with a leaked private key that has been updated.
 *
 * Number of time steps is determined by logl argument upon key generation, theoretically unbounded for
 *   log(l)/log(2) = 7 in the asymmetric product composition assuming integer time steps.
 *
 * Sum composition is based on underlying signing routine and the number of time steps is configurable by specifying
 * a tree height log(l)/log(2), yielding l time steps.
 */
//noinspection ScalaStyle

class SumComposition extends KesEd25519Blake2b256 {

  type SIG = (Array[Byte], Array[Byte], Array[Byte], Vector[(Array[Byte], Array[Byte])])
  type VK = (Array[Byte], Int)

  /**
   * Gets the public key in the sum composition
   *
   * @param keyTree binary tree for which the key is to be calculated
   * @return binary array public key
   */
  def generateVerificationKey(keyTree: KesBinaryTree): VK = keyTree match {
    case node: MerkleNode  => (node.witness, getKeyTime(keyTree))
    case leaf: SigningLeaf => (hash(leaf.witness), 0)
    case Empty             => (Array(), 0)
  }

  /**
   * Get the current time step of a sum composition key
   *
   * @param keyTree binary tree key
   * @return time step
   */
  def getKeyTime(keyTree: KesBinaryTree): Int =
    keyTree match {
      case MerkleNode(_, _, _, Empty, _: SigningLeaf)    => 1
      case MerkleNode(_, _, _, Empty, right: MerkleNode) => getKeyTime(right) + exp(right.height)
      case MerkleNode(_, _, _, left, Empty)              => getKeyTime(left)
      case _                                             => 0
    }

  /**
   * Generates keys in the sum composition, recursive functions construct the tree in steps and the output is
   * the leftmost branch
   *
   * @param seed input entropy for binary tree and keypair generation
   * @param i    height of tree
   * @return binary tree at time step 0
   */
  def generateSecretKey(seed: Array[Byte], height: Int): KesBinaryTree = {

    // generate the binary tree with the pseudorandom number generator
    def seedTree(seed: Array[Byte], height: Int): KesBinaryTree =
      if (height == 0) {
        SigningLeaf.tupled(sGenKeypair(seed))
      } else {
        val r = prng(seed)
        val left = seedTree(r._1, height - 1)
        val right = seedTree(r._2, height - 1)

        println(s"child witnesses, left: ${left} right: ${right}")
        MerkleNode(r._2, left.witness, right.witness, left, right)
      }

    //traverse down the tree to the leftmost leaf
    def reduceTree(fullTree: KesBinaryTree): KesBinaryTree =
      fullTree match {
        case MerkleNode(seed, witL, witR, nodeL, _) => MerkleNode(seed, witL, witR, reduceTree(nodeL), Empty)
        case leaf: SigningLeaf                      => leaf
        case _                                      => Empty
      }

    //executes the above functions in order
    val g = seedTree(seed, height)
    val f = reduceTree(g)
    println(s"head witness: ${g.witness}")
    g match {
      case MerkleNode(seed, witL, witR, nodeL, nodeR) => println(s"full tree matches? ${g.witness sameElements hash(nodeL.witness ++ nodeR.witness)}")
      case leaf: SigningLeaf                      => leaf
      case _ => Array()
    }
    f match {
      case MerkleNode(seed, witL, witR, nodeL, nodeR) => println(s"partial tree matches? ${f.witness sameElements hash(nodeL.witness ++ nodeR.witness)}")
      case leaf: SigningLeaf                      => leaf
      case _ => Array()
    }
    f
  }

  /**
   * Updates the key in the sum composition
   *
   * @param keyTree binary tree to be updated
   * @param step    time step key is to be updated to
   * @return updated key configuration
   */
  def updateKey(keyTree: KesBinaryTree, step: Int): KesBinaryTree = {

    /**
     * Evolves key a specified number of steps
     */
    def evolveKey(step: Int, input: KesBinaryTree): KesBinaryTree = {
      val halfTotalSteps = exp(input.height - 1)
      val shiftStep: Int => Int = (step: Int) => step % halfTotalSteps

      if (step >= halfTotalSteps) {
        input match {
          case MerkleNode(seed, witL, witR, _: SigningLeaf, Empty) =>
            MerkleNode(seed, witL, witR, Empty, SigningLeaf.tupled(sGenKeypair(seed)))

          case MerkleNode(seed, witL, witR, _: MerkleNode, Empty) =>
            MerkleNode(seed, witL, witR, Empty, evolveKey(shiftStep(step), generateSecretKey(seed, input.height - 1)))

          case MerkleNode(seed, witL, witR, Empty, right) =>
            MerkleNode(seed, witL, witR, Empty, evolveKey(shiftStep(step), right))

          case leaf: SigningLeaf => leaf
          case _                 => Empty
        }
      } else {
        input match {
          case MerkleNode(seed, witL, witR, left, Empty) =>
            MerkleNode(seed, witL, witR, evolveKey(shiftStep(step), left), Empty)

          case MerkleNode(seed, witL, witR, Empty, right) =>
            MerkleNode(seed, witL, witR, Empty, evolveKey(shiftStep(step), right))

          case leaf: SigningLeaf => leaf
          case _                 => Empty
        }
      }
    }

    val totalSteps = exp(keyTree.height)
    val keyTime = getKeyTime(keyTree)
    if (step < totalSteps && keyTime < step) {
      evolveKey(step, keyTree)
    } else {
      println("Time step error, key not updated")
      println("T: " + totalSteps.toString + ", key t:" + keyTime.toString + ", t:" + step.toString)
      keyTree
    }
  }

  /**
   * Signature in the sum composition
   *
   * @param keyTree secret key tree of the sum composition
   * @param m       message to be signed
   * @return byte array signature
   */
  def sign(keyTree: KesBinaryTree, m: Array[Byte]): SIG = {
    //loop that generates the signature of m and stacks up the witness path of the key
    def loop(
      keyTree: KesBinaryTree,
      W:       Vector[(Array[Byte], Array[Byte])] = Vector()
    ): SIG = keyTree match {
      case MerkleNode(_, _, _, left: MerkleNode, _)          => handleNode(left, W)
      case MerkleNode(_, _, witR, left: SigningLeaf, _)      => (left.vk, sSign(m, left.sk), witR, W)
      case MerkleNode(_, _, _, Empty, right: MerkleNode)     => handleNode(right, W)
      case MerkleNode(_, witL, _, Empty, right: SigningLeaf) => (right.vk, sSign(m, right.sk), witL, W)
      case leaf: SigningLeaf                                 => (leaf.vk, sSign(m, leaf.sk), Array(), W)
      case _                                                 => (Array(), Array(), Array(), Vector((Array(), Array())))
    }

    def handleNode(node: MerkleNode, accW: Vector[(Array[Byte], Array[Byte])]): SIG =
      loop(node, accW :+ (node.witnessLeft, node.witnessRight))

    loop(keyTree)
  }

  /**
   * Verify in the sum composition
   *
   * @param R   verification key of the sum composition
   * @param m   message corresponding to the signature
   * @param sig signature to be verified
   * @return true if the signature is valid false if otherwise
   */
  def verify(m: Array[Byte], kesVk: VK, kesSig: SIG): Boolean = {
    val (vkSign, sigSign, partnerWitness, merkleProof) = kesSig
    val (root: Array[Byte], step: Int) = kesVk

    // determine if the step corresponds to a right or left decision at each height
    val leftGoing: Int => Boolean = (level: Int) => {
      val g = ((step / exp(level)) % 2)
      println(s"step: $step, height: $level, value: $g")
      g == 0
    }

    lazy val verifyHead: Boolean =
      if (merkleProof.isEmpty) true
      else root sameElements hash(merkleProof.head._1 ++ merkleProof.head._2)

    @tailrec
    def verifyMerkle(W: Vector[(Array[Byte], Array[Byte])]): Boolean =
      if (W.length <= 1) true // terminating condition
      else if (leftGoing(W.length)) (W(0)._1 sameElements hash(W(1)._1 ++ W(1)._2)) && verifyMerkle(W.tail)
      else (W(0)._2 sameElements hash(W(1)._1 ++ W(1)._2)) && verifyMerkle(W.tail)

    lazy val verifyMerkleLeaf: Boolean =
      if (merkleProof.isEmpty) true
      else {
        println("---- start verifyMerkleLeaf ---")
        val (witL, witR) = merkleProof.last
        val g = if (leftGoing(1) && leftGoing(0)) {
          println("1")
          println(merkleProof.last)
          witL sameElements hash(hash(vkSign) ++ partnerWitness)
        } else if (leftGoing(1) && !leftGoing(0)) {
          println("2")
          witL sameElements hash(partnerWitness ++ hash(vkSign))
        } else if (!leftGoing(1) && leftGoing(0)) {
          println("3")
          witR sameElements hash(hash(vkSign) ++ partnerWitness)
        } else if (!leftGoing(1) && !leftGoing(0)) {
          println("4")
          witR sameElements hash(partnerWitness ++ hash(vkSign))
        } else {
          println("default")
          false
        }
        println("---- end verifyMerkleLeaf ---")
        g
      }


    val verifySigningLeaf =
      if (leftGoing(0)) root sameElements hash(hash(vkSign) ++ partnerWitness)
      else root sameElements hash(partnerWitness ++ hash(vkSign))

    val verifySign = sVerify(m, sigSign, vkSign)

    println("---- start verify ---")
    println(verifyHead)
    println(verifyMerkle(merkleProof))
    println(verifyMerkleLeaf)
    println(verifySign)
    println(verifySigningLeaf)
    println("---- end verify ---")

    if (merkleProof.nonEmpty) verifyHead && verifyMerkle(merkleProof) && verifyMerkleLeaf && verifySign
    else verifySigningLeaf && verifySign

  }
}

object jamesExample {
  val sc = new SumComposition()

  val myKey_init: sc.KesBinaryTree = sc.generateSecretKey(Array.fill(32)(0: Byte), 2)
  val myKey: sc.KesBinaryTree = sc.updateKey(myKey_init, 0)
  val m = Array.fill(32)(0: Byte)
  val vk = sc.generateVerificationKey(myKey)
  val sig = sc.sign(myKey, m)

  def main(args: Array[String]): Unit = {
    println(s"key: $myKey")
    println(s"vk: $vk")
    println(s"sig: $sig")
    println(s" verification: ${sc.verify(m, vk, sig)}")
  }
}
