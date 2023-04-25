package co.topl.crypto.utils

import co.topl.crypto.models._

object KesTestHelper {

  object PrivateKeyConstructor {
    // args has a boolean for each entry corresponding to false <=> left/zero and true <=> right/one,
    // false-true corresponds to left or right child node with respect to the parent being constructed
    // witness elements and seeds should be provided for each level of the tree in order of highest to lowest
    // sk and vk are the leaf (lowest) level private and public keys
    type Args = (Boolean, (Array[Byte], Array[Byte], Array[Byte]))

    def build(sk: Array[Byte], vk: Array[Byte], args: Args*): KesBinaryTree =
      args.length match {
        case 0 =>
          KesBinaryTree.SigningLeaf(sk, vk)
        case _ =>
          if (args.head._1) {
            KesBinaryTree.MerkleNode(
              args.head._2._1,
              args.head._2._2,
              args.head._2._3,
              KesBinaryTree.Empty(),
              this.build(sk, vk, args.tail: _*)
            )
          } else {
            KesBinaryTree.MerkleNode(
              args.head._2._1,
              args.head._2._2,
              args.head._2._3,
              this.build(sk, vk, args.tail: _*),
              KesBinaryTree.Empty()
            )
          }
      }
  }

  def areEqual(a: SecretKeyKesProduct, b: Any): Boolean =
    (a, b) match {
      case (
            SecretKeyKesProduct(
              superTree_a,
              subTree_a,
              nextSubSeed_a,
              subSignature_a,
              offset_a
            ),
            SecretKeyKesProduct(
              superTree_b,
              subTree_b,
              nextSubSeed_b,
              subSignature_b,
              offset_b
            )
          ) =>
        areEqual(superTree_a, superTree_b) &&
        areEqual(subTree_a, subTree_b) &&
        nextSubSeed_a.sameElements(nextSubSeed_b) &&
        subSignature_a == subSignature_b &&
        offset_a == offset_b
      case _ => false
    }

  def areEqual(a: KesBinaryTree, b: Any): Boolean =
    (a, b) match {
      case (
            KesBinaryTree.MerkleNode(
              seed_a: Array[Byte],
              witnessLeft_a: Array[Byte],
              witnessRight_a: Array[Byte],
              left_a: KesBinaryTree,
              right_a: KesBinaryTree
            ),
            KesBinaryTree.MerkleNode(
              seed_b: Array[Byte],
              witnessLeft_b: Array[Byte],
              witnessRight_b: Array[Byte],
              left_b: KesBinaryTree,
              right_b: KesBinaryTree
            )
          ) =>
        seed_a.sameElements(seed_b) &&
        witnessLeft_a.sameElements(witnessLeft_b) &&
        witnessRight_a.sameElements(witnessRight_b) &&
        areEqual(left_a, left_b) &&
        areEqual(right_a, right_b)
      case (KesBinaryTree.Empty(), KesBinaryTree.Empty()) =>
        true
      case (KesBinaryTree.SigningLeaf(sk_a, vk_a), KesBinaryTree.SigningLeaf(sk_b, vk_b)) =>
        sk_a.sameElements(sk_b) &&
        vk_a.sameElements(vk_b)
      case _ =>
        false
    }

}
