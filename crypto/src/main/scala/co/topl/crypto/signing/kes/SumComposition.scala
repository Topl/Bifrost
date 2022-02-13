package co.topl.crypto.signing.kes

import co.topl.models.utility.KesBinaryTree
import co.topl.models.utility.KesBinaryTree.{Empty(), MerkleNode, SigningLeaf}
import java.security.SecureRandom
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

  private val random = new SecureRandom()

  /**
   * Get the current time step of a sum composition key
   *
   * @param keyTree binary tree key
   * @return time step
   */
  private[signing] def getKeyTime(keyTree: SK): Int =
    keyTree match {
      case MerkleNode(_, _, _, Empty(), _: SigningLeaf)    => 1
      case MerkleNode(_, _, _, Empty(), right: MerkleNode) => getKeyTime(right) + exp(getTreeHeight(right))
      case MerkleNode(_, _, _, left, Empty())              => getKeyTime(left)
      case _                                             => 0
    }

  /**
   * Gets the public key in the sum composition
   *
   * @param keyTree binary tree for which the key is to be calculated
   * @return binary array public key
   */
  private[signing] def generateVerificationKey(keyTree: SK): VK = {
    val h = keyTree match {
      case node: MerkleNode  => (witness(node), getKeyTime(keyTree))
      case leaf: SigningLeaf => (witness(leaf), 0)
      case Empty()             => (Array.fill(hashBytes)(0: Byte), 0)
    }
//    println(s"---------------------------start sum verification key ----------------")
//    println(s"verification witness: ${Base58.encode(h._1)}, keyTime: ${h._2}")
//    println(s"---------------------------end sum verification key ----------------")
    h
  }

  /**
   * Generates keys in the sum composition, recursive functions construct the tree in steps and the output is
   * the leftmost branch
   *
   * @param seed input entropy for binary tree and keypair generation
   * @param i    height of tree
   * @return binary tree at time step 0
   */
  private[signing] def generateSecretKey(seed: Array[Byte], height: Int): SK = {

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
        case MerkleNode(seed, witL, witR, nodeL, nodeR) =>
          eraseOldNode(nodeR)
          MerkleNode(seed, witL, witR, reduceTree(nodeL), Empty())
        case leaf: SigningLeaf => leaf
        case _                 => Empty()
      }

    //executes the above functions in order
    val out = reduceTree(seedTree(seed, height))
    random.nextBytes(seed)
    out
  }

  /**
   * Updates the key in the sum composition
   *
   * @param keyTree binary tree to be updated
   * @param step    time step key is to be updated to
   * @return updated key configuration
   */
  private[signing] def updateKey(keyTree: SK, step: Int): SK = {
    val totalSteps = exp(getTreeHeight(keyTree))
    val keyTime = getKeyTime(keyTree)
    if (step == 0) keyTree
    else if (step < totalSteps && keyTime < step) {
      evolveKey(keyTree, step)
    } else {
      throw new Error(
        s"Update error - Max steps: $totalSteps, current step: $keyTime, requested increase: $step"
      )
    }
  }

  private def eraseOldNode(node: KesBinaryTree): Unit =
    node match {
      case merkleNode: MerkleNode =>
        random.nextBytes(merkleNode.seed)
        random.nextBytes(merkleNode.witnessLeft)
        random.nextBytes(merkleNode.witnessRight)
        merkleNode.left match {
          case l: MerkleNode => eraseOldNode(l)
          case l: SigningLeaf =>
            random.nextBytes(l.sk)
            random.nextBytes(l.vk)
          case _ =>
        }
        merkleNode.right match {
          case r: MerkleNode => eraseOldNode(r)
          case r: SigningLeaf =>
            random.nextBytes(r.sk)
            random.nextBytes(r.vk)
          case _ =>
        }
      case leaf: SigningLeaf =>
        random.nextBytes(leaf.sk)
        random.nextBytes(leaf.vk)
      case _ =>
    }

  def eraseKey(input: KesBinaryTree): Unit = eraseOldNode(input)

  /**
   * Evolves key a specified number of steps
   */
  private[signing] def evolveKey(input: KesBinaryTree, step: Int): KesBinaryTree = {
    val halfTotalSteps = exp(getTreeHeight(input) - 1)
    val shiftStep: Int => Int = (step: Int) => step % halfTotalSteps

    if (step >= halfTotalSteps) {
      input match {
        case MerkleNode(seed, witL, witR, oldLeaf: SigningLeaf, Empty()) =>
          val newNode =
            MerkleNode(Array.fill(seed.length)(0: Byte), witL, witR, Empty(), SigningLeaf.tupled(sGenKeypair(seed)))
          eraseOldNode(oldLeaf)
          random.nextBytes(seed)
          newNode
        case MerkleNode(seed, witL, witR, oldNode: MerkleNode, Empty()) =>
          val newNode = MerkleNode(
            Array.fill(seed.length)(0: Byte),
            witL,
            witR,
            Empty(),
            evolveKey(generateSecretKey(seed, getTreeHeight(input) - 1), shiftStep(step))
          )
          eraseOldNode(oldNode)
          random.nextBytes(seed)
          newNode
        case MerkleNode(seed, witL, witR, Empty(), right) =>
          MerkleNode(seed, witL, witR, Empty(), evolveKey(right, shiftStep(step)))

        case leaf: SigningLeaf => leaf
        case _                 => Empty()
      }
    } else {
      input match {
        case MerkleNode(seed, witL, witR, left, Empty()) =>
          MerkleNode(seed, witL, witR, evolveKey(left, shiftStep(step)), Empty())

        case MerkleNode(seed, witL, witR, Empty(), right) =>
          MerkleNode(seed, witL, witR, Empty(), evolveKey(right, shiftStep(step)))

        case leaf: SigningLeaf => leaf
        case _                 => Empty()
      }
    }
  }

  /**
   * Signature in the sum composition
   *
   * @param keyTree secret key tree of the sum composition
   * @param m       message to be signed
   * @return byte array signature
   */
  private[signing] def sign(keyTree: SK, m: Array[Byte]): SIG = {
    //loop that generates the signature of m and stacks up the witness path of the key
    @tailrec
    def loop(
      keyTree: KesBinaryTree,
      W:       Vector[Array[Byte]] = Vector()
    ): SIG = keyTree match {
      case MerkleNode(_, witL, _, Empty(), right) => loop(right, witL.clone() +: W)
      case MerkleNode(_, _, witR, left, _)      => loop(left, witR.clone() +: W)
      case leaf: SigningLeaf                    => (leaf.vk.clone(), sSign(m, leaf.sk).clone(), W)
      case _                                    => (Array.fill(pkBytes)(0: Byte), Array.fill(sigBytes)(0: Byte), Vector(Array()))
    }
    loop(keyTree)
  }

  /**
   * Verify in the sum composition
   * @param kesSig signature to be verified
   * @param m message corresponding to the signature
   * @param kesVk verification key of the sum composition
   * @return true if the signature is valid false if otherwise
   */
  private[signing] def verify(kesSig: SIG, m: Array[Byte], kesVk: VK): Boolean = {
    val (vkSign, sigSign, merkleProof) = kesSig
    val (root: Array[Byte], step: Int) = kesVk

    // determine if the step corresponds to a right or left decision at each height
    val leftGoing: Int => Boolean = (level: Int) => ((step / exp(level)) % 2) == 0

    def verifyMerkle(W: Vector[Array[Byte]]): Boolean =
      if (W.isEmpty()) Empty()Witness
      else if (W.length == 1) singleWitness(W.head)
      else if (leftGoing(0)) multiWitness(W.tail, hash(vkSign), W.head, 1)
      else multiWitness(W.tail, W.head, hash(vkSign), 1)

    def Empty()Witness: Boolean = root sameElements hash(vkSign)

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
      if (witnessList.isEmpty()) root sameElements hash(witnessLeft ++ witnessRight)
      else if (leftGoing(index))
        multiWitness(witnessList.tail, hash(witnessLeft ++ witnessRight), witnessList.head, index + 1)
      else multiWitness(witnessList.tail, witnessList.head, hash(witnessLeft ++ witnessRight), index + 1)

    val verifySign = sVerify(m, sigSign, vkSign)

    verifyMerkle(merkleProof) && verifySign

  }
}
