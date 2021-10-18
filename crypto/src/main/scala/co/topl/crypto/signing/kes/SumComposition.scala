package co.topl.crypto.signing.kes

import co.topl.models.utility.{BinaryTree, Empty, Leaf, Node}

import scala.math.BigInt

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

  /**
   * Gets the public key in the sum composition
   * @param t binary tree for which the key is to be calculated
   * @return binary array public key
   */
  def generateVerificationKey(t: BinaryTree[Array[Byte]]): Array[Byte] =
    t match {
      case n: Node[Array[Byte]] =>
        val pk0 = n.v.slice(seedBytes, seedBytes + pkBytes)
        val pk1 = n.v.slice(seedBytes + pkBytes, seedBytes + 2 * pkBytes)
        hash(pk0 ++ pk1)
      case l: Leaf[Array[Byte]] =>
        hash(
          hash(l.v.slice(seedBytes, seedBytes + pkBytes)) ++ hash(l.v.slice(seedBytes, seedBytes + pkBytes))
        )
      case _ => Array()
    }

  /**
   * Generates keys in the sum composition, recursive functions construct the tree in steps and the output is
   * the leftmost branch
   * @param seed input entropy for binary tree and keypair generation
   * @param i height of tree
   * @return binary tree at time step 0
   */
  def generateSecretKey(seed: Array[Byte], i: Int): BinaryTree[Array[Byte]] = {

    // generate the binary tree with the pseudorandom number generator
    def seedTree(seed: Array[Byte], i: Int): BinaryTree[Array[Byte]] =
      if (i == 0) {
        Leaf(sGenKeypair(seed))
      } else {
        val r = prng(seed)
        Node(r._2, seedTree(r._1, i - 1), seedTree(r._2, i - 1))
      }

//    // generates the SIG keypairs on each leaf
//    def populateLeaf(t: BinaryTree[Array[Byte]]): BinaryTree[Array[Byte]] =
//      t match {
//        case n: Node[Array[Byte]] =>
//          Node(n.v, populateLeaf(n.l), populateLeaf(n.r))
//        case l: Leaf[Array[Byte]] =>
//          Leaf(l.v ++ sGenKeypair(l.v))
//        case _ =>
//          Empty
//      }

    // generates the Merkle tree of the public keys and stores the hash values on each node
    def merkleVerificationKey(t: BinaryTree[Array[Byte]]): BinaryTree[Array[Byte]] = {
      def loop(t: BinaryTree[Array[Byte]]): BinaryTree[Array[Byte]] =
        t match {
          case n: Node[Array[Byte]] =>
            var r0: Array[Byte] = Array()
            var sk0: Array[Byte] = Array()
            var pk0: Array[Byte] = Array()
            var pk00: Array[Byte] = Array()
            var pk01: Array[Byte] = Array()
            var r1: Array[Byte] = Array()
            var sk1: Array[Byte] = Array()
            var pk1: Array[Byte] = Array()
            var pk10: Array[Byte] = Array()
            var pk11: Array[Byte] = Array()

            var leftVal: Array[Byte] = Array()
            var rightVal: Array[Byte] = Array()
            var leafLevel = false

            val left = loop(n.l) match {
              case nn: Node[Array[Byte]] =>
                leftVal = nn.v
                nn
              case ll: Leaf[Array[Byte]] =>
                leafLevel = true
                leftVal = ll.v
                ll
            }

            val right = loop(n.r) match {
              case nn: Node[Array[Byte]] =>
                rightVal = nn.v
                nn
              case ll: Leaf[Array[Byte]] =>
                leafLevel = true
                rightVal = ll.v
                ll
            }

            if (leafLevel) {
              r0 = leftVal.slice(0, seedBytes)
              sk0 = leftVal.slice(seedBytes, seedBytes + skBytes)
              pk0 = leftVal.slice(seedBytes + skBytes, seedBytes + skBytes + pkBytes)
              r1 = rightVal.slice(0, seedBytes)
              sk1 = rightVal.slice(seedBytes, seedBytes + skBytes)
              pk1 = rightVal.slice(seedBytes + skBytes, seedBytes + skBytes + pkBytes)
              // needed?
              //assert(n.v sameElements r1)
              Node(n.v ++ hash(pk0) ++ hash(pk1), Leaf(sk0 ++ pk0), Leaf(sk1 ++ pk1))
            } else {
              pk00 = leftVal.slice(seedBytes, seedBytes + pkBytes)
              pk01 = leftVal.slice(seedBytes + pkBytes, seedBytes + 2 * pkBytes)
              pk10 = rightVal.slice(seedBytes, seedBytes + pkBytes)
              pk11 = rightVal.slice(seedBytes + pkBytes, seedBytes + 2 * pkBytes)

              Node(n.v ++ hash(pk00 ++ pk01) ++ hash(pk10 ++ pk11), left, right)
            }

          case l: Leaf[Array[Byte]] =>
            l
          case _ =>
            Empty
        }


      t match {
        case n: Node[Array[Byte]] =>
          loop(n)
        case l: Leaf[Array[Byte]] => l
        case _ =>
          Empty
      }
    }

    //traverse down the tree to the leftmost leaf
    def reduceTree(t: BinaryTree[Array[Byte]]): BinaryTree[Array[Byte]] =
      t match {
        case n: Node[Array[Byte]] =>
          Node(n.v, reduceTree(n.l), Empty)
        case l: Leaf[Array[Byte]] =>
          l
        case _ =>
          Empty
      }

    //executes the above functions in order
    reduceTree(merkleVerificationKey(seedTree(seed, i)))
  }

  /**
   * Verify a public key with a binary tree
   * @param t binary tree that contains Merkle tree hash values
   * @param pk root of the Merkle tree
   * @return true if pk is the root of the Merkle tree, false if otherwise
   */
  def sumCompositionVerifyKeyPair(t: BinaryTree[Array[Byte]], pk: Array[Byte]): Boolean = {
    //loops through the tree to verify Merkle witness path
    def loop(t: BinaryTree[Array[Byte]]): Boolean =
      t match {
        case n: Node[Array[Byte]] =>
          var pk0: Array[Byte] = Array()
          var pk00: Array[Byte] = Array()
          var pk01: Array[Byte] = Array()
          var pk1: Array[Byte] = Array()
          var pk10: Array[Byte] = Array()
          var pk11: Array[Byte] = Array()
          val left = n.l match {
            case nn: Node[Array[Byte]] =>
              pk00 = nn.v.slice(seedBytes, seedBytes + pkBytes)
              pk01 = nn.v.slice(seedBytes + pkBytes, seedBytes + 2 * pkBytes)
              pk0 = hash(pk00 ++ pk01)
              loop(nn) && (pk0 sameElements n.v.slice(seedBytes, seedBytes + pkBytes))
            case ll: Leaf[Array[Byte]] =>
              hash(ll.v.slice(skBytes, skBytes + pkBytes)) sameElements n.v.slice(seedBytes, seedBytes + pkBytes)
            case _ => true
          }
          val right = n.r match {
            case nn: Node[Array[Byte]] =>
              pk10 = nn.v.slice(seedBytes, seedBytes + pkBytes)
              pk11 = nn.v.slice(seedBytes + pkBytes, seedBytes + 2 * pkBytes)
              pk1 = hash(pk10 ++ pk11)
              loop(nn) && (pk1 sameElements n.v.slice(seedBytes + pkBytes, seedBytes + 2 * pkBytes))
            case ll: Leaf[Array[Byte]] =>
              hash(ll.v.slice(skBytes, skBytes + pkBytes)) sameElements n.v.slice(
                seedBytes + pkBytes,
                seedBytes + 2 * pkBytes
              )
            case _ => true
          }
          left && right
        case l: Leaf[Array[Byte]] =>
          hash(
            hash(l.v.slice(skBytes, skBytes + pkBytes)) ++ hash(l.v.slice(skBytes, skBytes + pkBytes))
          ) sameElements pk
        case _ => false
      }
    (pk sameElements generateVerificationKey(t)) && loop(t)
  }

//  /**
//   * Updates the key in the sum composition
//   * @param key binary tree to be updated
//   * @param t time step key is to be updated to
//   * @return updated key to be written to key
//   */
//  def sumCompositionUpdate(key: BinaryTree[Array[Byte]], t: Int): BinaryTree[Array[Byte]] = {
//    //checks if the sub tree is right most
//    def isRightBranch(t: BinaryTree[Array[Byte]]): Boolean =
//      t match {
//        case n: Node[Array[Byte]] =>
//          val left = n.l match {
//            case _: Node[Array[Byte]] => false
//            case _: Leaf[Array[Byte]] => false
//            case _                    => true
//          }
//          val right = n.r match {
//            case n: Node[Array[Byte]] => isRightBranch(n)
//            case _: Leaf[Array[Byte]] => true
//            case _                    => false
//          }
//          left && right
//        case _: Leaf[Array[Byte]] => false
//        case _                    => false
//      }
//
//    //main loop that steps the tree to the next time step
//    def loop(t: BinaryTree[Array[Byte]]): BinaryTree[Array[Byte]] =
//      t match {
//        case n: Node[Array[Byte]] =>
//          var leftIsEmpty = false
//          var leftIsLeaf = false
//          var leftIsNode = false
//          var leftVal: Array[Byte] = Array()
//          var rightIsEmpty = false
//          var rightIsLeaf = false
//          var rightIsNode = false
//          var rightVal: Array[Byte] = Array()
//          val left = n.l match {
//            case n: Node[Array[Byte]] => leftIsNode = true; leftVal = n.v; n
//            case l: Leaf[Array[Byte]] => leftIsLeaf = true; leftVal = l.v; l
//            case _                    => leftIsEmpty = true; n.l
//          }
//          val right = n.r match {
//            case n: Node[Array[Byte]] => rightIsNode = true; rightVal = n.v; n
//            case l: Leaf[Array[Byte]] => rightIsLeaf = true; rightVal = l.v; l
//            case _                    => rightIsEmpty = true; n.r
//          }
//          val cutBranch = isRightBranch(left)
//          if (rightIsEmpty && leftIsLeaf) {
//            left.toSeqInorder.foreach(random.nextBytes)
//            val sk = sGenKeypair(n.v.slice(0, seedBytes)).slice(skBytes, skBytes + pkBytes)
//
//            // is this needed?
////            assert(
////              hash(keyPair.slice(skBytes, skBytes + pkBytes)) sameElements n.v
////                .slice(seedBytes + pkBytes, seedBytes + 2 * pkBytes)
////            )
//
//            Node(n.v, Empty, Leaf(sk))
//          } else if (cutBranch) {
//            left.toSeqInorder.foreach(random.nextBytes)
//            Node(n.v, Empty, generateSecretKey(n.v.slice(0, seedBytes), n.height - 1))
//          } else if (leftIsNode && rightIsEmpty) {
//            Node(n.v, loop(left), Empty)
//          } else if (leftIsEmpty && rightIsNode) {
//            Node(n.v, Empty, loop(right))
//          } else {
//            n
//          }
//        case l: Leaf[Array[Byte]] => l
//        case _                    => t
//      }
//
//    val T = exp(key.height)
//    val keyTime = getKeyTime(key)
//    //steps key through time steps one at a time until key step == t
//    if (t < T && keyTime < t) {
//      var tempKey = key
//      for (_ <- keyTime + 1 to t)
//        tempKey = loop(tempKey)
//      tempKey
//    } else {
//      println("Time step error, key not updated")
//      println("T: " + T.toString + ", key t:" + keyTime.toString + ", t:" + t.toString)
//      key
//    }
//  }

  /**
   * Evolves key a specified number of steps
   * @param step number of steps to evolve
   * @param input starting key configuration
   * @return an updated key configuration
   */
  private def evolveKey(step: Int, input: BinaryTree[Array[Byte]]): BinaryTree[Array[Byte]] =
    input match {
      case n: Node[Array[Byte]] =>
        var leftIsEmpty = false
        var leftIsLeaf = false
        var leftIsNode = false
        var leftVal: Array[Byte] = Array()
        var rightIsEmpty = false
        var rightIsLeaf = false
        var rightIsNode = false
        var rightVal: Array[Byte] = Array()

        val left = n.l match {
          case n: Node[Array[Byte]] => leftIsNode = true; leftVal = n.v; n
          case l: Leaf[Array[Byte]] => leftIsLeaf = true; leftVal = l.v; l
          case _                    => leftIsEmpty = true; n.l
        }
        val right = n.r match {
          case n: Node[Array[Byte]] => rightIsNode = true; rightVal = n.v; n
          case l: Leaf[Array[Byte]] => rightIsLeaf = true; rightVal = l.v; l
          case _                    => rightIsEmpty = true; n.r
        }

        val e = exp(n.height - 1)
        val nextStep = step % e

        if (step >= e) {
          if (rightIsEmpty && leftIsLeaf) {
            left.toSeqInorder.foreach(random.nextBytes)
            val keyPair = sGenKeypair(n.v.slice(0, seedBytes))

            //                assert(
            //                  hash(keyPair.slice(skBytes, skBytes + pkBytes)) sameElements n.v
            //                    .slice(seedBytes + pkBytes, seedBytes + 2 * pkBytes)
            //                )

            Node(n.v, Empty, Leaf(keyPair))

          } else if (leftIsEmpty && rightIsNode) {
            Node(n.v, Empty, evolveKey(nextStep, right))

          } else if (rightIsEmpty && leftIsNode) {
            left.toSeqInorder.foreach(random.nextBytes)
            val subKey = generateSecretKey(n.v.slice(0, seedBytes), n.height - 1)
            Node(n.v, Empty, evolveKey(nextStep, subKey))

          } else {
            n
          }

        } else {
          if (rightIsEmpty) {
            Node(n.v, evolveKey(nextStep, left), Empty)
          } else if (leftIsEmpty) {
            Node(n.v, Empty, evolveKey(nextStep, right))
          } else {
            n
          }
        }
      case l: Leaf[Array[Byte]] => l
      case _                    => input
    }

  /**
   * Updates the key in the sum composition
   * @param key binary tree to be updated
   * @param t time step key is to be updated to
   * @return updated key to be written to key
   */
  def updateKey(key: BinaryTree[Array[Byte]], t: Int): BinaryTree[Array[Byte]] = {
    val T = exp(key.height)
    val keyTime = getKeyTime(key)
    if (t < T && keyTime < t) {

      evolveKey(t, key)
    } else {
      println("Time step error, key not updated")
      println("T: " + T.toString + ", key t:" + keyTime.toString + ", t:" + t.toString)
      key
    }
  }

  /**
   * Signature in the sum composition
   * @param sk secret key tree of the sum composition
   * @param m message to be signed
   * @param step  current time step of signing key sk
   * @return byte array signature
   */
  def sign(sk: BinaryTree[Array[Byte]], m: Array[Byte], step: Int): Array[Byte] = {
//    assert(step == getKeyTime(sk))
//    assert(sumCompositionVerifyKeyPair(sk, generateVerificationKey(sk)))
    val stepBytesBigInt = BigInt(step).toByteArray
    val stepBytes = Array.fill(seedBytes - stepBytesBigInt.length)(0x00.toByte) ++ stepBytesBigInt

    //loop that generates the signature of m++step and stacks up the witness path of the key
    def loop(t: BinaryTree[Array[Byte]]): Array[Byte] =
      t match {
        case n: Node[Array[Byte]] =>
          val left = n.l match {
            case nn: Node[Array[Byte]] =>
              loop(nn)
            case ll: Leaf[Array[Byte]] =>
              sSign(m ++ stepBytes, ll.v.slice(0, skBytes)) ++ ll.v.slice(skBytes, skBytes + pkBytes) ++ stepBytes
            case _ => Array()
          }
          val right = n.r match {
            case nn: Node[Array[Byte]] =>
              loop(nn)
            case ll: Leaf[Array[Byte]] =>
              sSign(m ++ stepBytes, ll.v.slice(0, skBytes)) ++ ll.v.slice(skBytes, skBytes + pkBytes) ++ stepBytes
            case _ => Array()
          }
          left ++ right ++ n.v.slice(seedBytes, seedBytes + 2 * pkBytes)

        case l: Leaf[Array[Byte]] =>
          sSign(m ++ stepBytes, l.v.slice(0, skBytes)) ++ l.v.slice(skBytes, skBytes + pkBytes) ++ stepBytes ++ hash(
            l.v.slice(skBytes, skBytes + pkBytes)
          ) ++ hash(l.v.slice(skBytes, skBytes + pkBytes))

        case _ =>
          Array()
      }

    loop(sk)
  }

  /**
   * Verify in the sum composition
   * @param pk public key of the sum composition
   * @param m message corresponding to the signature
   * @param sig signature to be verified
   * @return true if the signature is valid false if otherwise
   */
  def verify(pk: Array[Byte], m: Array[Byte], sig: Array[Byte], t: Int): Boolean = {
    val pkSeq = sig.drop(sigBytes + pkBytes + seedBytes)
    val stepBytes = sig.slice(sigBytes + pkBytes, sigBytes + pkBytes + seedBytes)
    val step = BigInt(stepBytes)
    var pkLogic = true
    if (step % 2 == 0) {
      pkLogic &= hash(sig.slice(sigBytes, sigBytes + pkBytes)) sameElements pkSeq.slice(0, pkBytes)
    } else {
      pkLogic &= hash(sig.slice(sigBytes, sigBytes + pkBytes)) sameElements pkSeq.slice(pkBytes, 2 * pkBytes)
    }
    for (i <- 0 to pkSeq.length / pkBytes - 4 by 2) {
      val pk0: Array[Byte] = pkSeq.slice((i + 2) * pkBytes, (i + 3) * pkBytes)
      val pk00: Array[Byte] = pkSeq.slice(i * pkBytes, (i + 1) * pkBytes)
      val pk01: Array[Byte] = pkSeq.slice((i + 1) * pkBytes, (i + 2) * pkBytes)
      val pk1: Array[Byte] = pkSeq.slice((i + 3) * pkBytes, (i + 4) * pkBytes)
      val pk10: Array[Byte] = pkSeq.slice(i * pkBytes, (i + 1) * pkBytes)
      val pk11: Array[Byte] = pkSeq.slice((i + 1) * pkBytes, (i + 2) * pkBytes)
      if ((step.toInt / exp(i / 2 + 1)) % 2 == 0) {
        pkLogic &= pk0 sameElements hash(pk00 ++ pk01)
      } else {
        pkLogic &= pk1 sameElements hash(pk10 ++ pk11)
      }
    }
    pkLogic &= pk sameElements hash(pkSeq.slice(pkSeq.length - 2 * pkBytes, pkSeq.length))
    sVerify(
      m ++ stepBytes,
      sig.slice(0, sigBytes),
      sig.slice(sigBytes, sigBytes + pkBytes)
    ) && pkLogic && step.toInt == t
  }

  /**
   * Get the current time step of a sum composition key
   * @param key binary tree key
   * @return time step
   */
  def getKeyTime(key: BinaryTree[Array[Byte]]): Int =
    key match {
      case n: Node[Array[Byte]] =>
        val left = n.l match {
          case n: Node[Array[Byte]] => getKeyTime(n)
          case _: Leaf[Array[Byte]] => 0
          case _                    => 0
        }
        val right = n.r match {
          case n: Node[Array[Byte]] => getKeyTime(n) + exp(n.height)
          case _: Leaf[Array[Byte]] => 1
          case _                    => 0
        }
        left + right
      case _: Leaf[Array[Byte]] => 0
      case _                    => 0
    }

}
