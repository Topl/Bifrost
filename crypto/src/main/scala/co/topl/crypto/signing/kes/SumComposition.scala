package co.topl.crypto.signing.kes

import co.topl.models.utility.KesBinaryTree
import co.topl.models.utility.KesBinaryTree.{Empty, MerkleNode, SigningLeaf}

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

  override type SIG = (Array[Byte], Array[Byte], Vector[Array[Byte]])
  override type VK = (Array[Byte], Int)
  override type SK = KesBinaryTree

  /**
   * Gets the public key in the sum composition
   *
   * @param keyTree binary tree for which the key is to be calculated
   * @return binary array public key
   */
  protected def generateVerificationKey(keyTree: KesBinaryTree): VK = keyTree match {
    case node: MerkleNode  => (witness(node), getKeyTime(keyTree))
    case leaf: SigningLeaf => (witness(leaf), 0)
    case Empty             => (Array.fill(hashBytes)(0: Byte), 0)
  }

  /**
   * Get the current time step of a sum composition key
   *
   * @param keyTree binary tree key
   * @return time step
   */
  protected def getKeyTime(keyTree: KesBinaryTree): Int =
    keyTree match {
      case MerkleNode(_, _, _, Empty, _: SigningLeaf)    => 1
      case MerkleNode(_, _, _, Empty, right: MerkleNode) => getKeyTime(right) + exp(getTreeHeight(right))
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
  protected def generateSecretKey(seed: Array[Byte], height: Int): KesBinaryTree = {

    // generate the binary tree with the pseudorandom number generator
    def seedTree(seed: Array[Byte], height: Int): KesBinaryTree =
      if (height == 0) {
        SigningLeaf.tupled(sGenKeypair(seed))
      } else {
        val r = prng(seed)
        val left = seedTree(r._1, height - 1)
        val right = seedTree(r._2, height - 1)
        MerkleNode(r._2, witness(left), witness(right), left, right)
      }

    //traverse down the tree to the leftmost leaf
    def reduceTree(fullTree: KesBinaryTree): KesBinaryTree =
      fullTree match {
        case MerkleNode(seed, witL, witR, nodeL, _) => MerkleNode(seed, witL, witR, reduceTree(nodeL), Empty)
        case leaf: SigningLeaf                      => leaf
        case _                                      => Empty
      }

    //executes the above functions in order
    reduceTree(seedTree(seed, height))
  }

  /**
   * Updates the key in the sum composition
   *
   * @param keyTree binary tree to be updated
   * @param step    time step key is to be updated to
   * @return updated key configuration
   */
  protected def updateKey(keyTree: KesBinaryTree, step: Int): KesBinaryTree = {

    /**
     * Evolves key a specified number of steps
     */
    def evolveKey(step: Int, input: KesBinaryTree): KesBinaryTree = {
      val halfTotalSteps = exp(getTreeHeight(input) - 1)
      val shiftStep: Int => Int = (step: Int) => step % halfTotalSteps

      if (step >= halfTotalSteps) {
        input match {
          case MerkleNode(seed, witL, witR, _: SigningLeaf, Empty) =>
            MerkleNode(seed, witL, witR, Empty, SigningLeaf.tupled(sGenKeypair(seed)))

          case MerkleNode(seed, witL, witR, _: MerkleNode, Empty) =>
            MerkleNode(
              seed,
              witL,
              witR,
              Empty,
              evolveKey(shiftStep(step), generateSecretKey(seed, getTreeHeight(input) - 1))
            )

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

    val totalSteps = exp(getTreeHeight(keyTree))
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
  protected def sign(keyTree: KesBinaryTree, m: Array[Byte]): SIG = {
    //loop that generates the signature of m and stacks up the witness path of the key
    @tailrec
    def loop(
      keyTree: KesBinaryTree,
      W:       Vector[Array[Byte]] = Vector()
    ): SIG = keyTree match {
      case MerkleNode(_, witL, _, Empty, right) => loop(right, witL +: W)
      case MerkleNode(_, _, witR, left, _)      => loop(left, witR +: W)
      case leaf: SigningLeaf                    => (leaf.vk, sSign(m, leaf.sk), W)
      case _                                    => (Array.fill(pkBytes)(0: Byte), Array.fill(sigBytes)(0: Byte), Vector(Array()))
    }

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
  protected def verify(kesSig: SIG, m: Array[Byte], kesVk: VK): Boolean = {
    val (vkSign, sigSign, merkleProof) = kesSig
    val (root: Array[Byte], step: Int) = kesVk

    // determine if the step corresponds to a right or left decision at each height
    val leftGoing: Int => Boolean = (level: Int) => ((step / exp(level)) % 2) == 0

    def verifyMerkle(W: Vector[Array[Byte]]): Boolean =
      if (W.isEmpty) emptyWitness
      else if (W.length == 1) singleWitness(W.head)
      else if (leftGoing(0)) multiWitness(W.tail, hash(vkSign), W.head, 1)
      else multiWitness(W.tail, W.head, hash(vkSign), 1)

    def emptyWitness: Boolean = root sameElements hash(vkSign)

    def singleWitness(witness: Array[Byte]): Boolean =
      if (leftGoing(0)) root sameElements hash(hash(vkSign) ++ witness)
      else root sameElements hash(witness ++ hash(vkSign))

    @tailrec
    def multiWitness(
      witnessList:  Vector[Array[Byte]],
      witnessLeft:  Array[Byte],
      witnessRight: Array[Byte],
      index:        Int
    ): Boolean =
      if (witnessList.isEmpty) root sameElements hash(witnessLeft ++ witnessRight)
      else if (leftGoing(index))
        multiWitness(witnessList.tail, hash(witnessLeft ++ witnessRight), witnessList.head, index + 1)
      else multiWitness(witnessList.tail, witnessList.head, hash(witnessLeft ++ witnessRight), index + 1)

    val verifySign = sVerify(m, sigSign, vkSign)

    verifyMerkle(merkleProof) && verifySign

  }
}
