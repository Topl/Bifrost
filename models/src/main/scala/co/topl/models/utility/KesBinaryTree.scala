package co.topl.models.utility

import co.topl.models.Bytes

sealed trait KesBinaryTree

object KesBinaryTree {

  object PrivateKeyConstructor {
    // args has a boolean for each entry corresponding to false <=> left/zero and true <=> right/one,
    // false-true corresponds to left or right child node with respect to the parent being constructed
    // witness elements and seeds should be provided for each level of the tree in order of highest to lowest
    // sk and vk are the leaf (lowest) level private and public keys
    type Args = (Boolean, (Bytes, Bytes, Bytes))

    def build(sk: Bytes, vk: Bytes, args: Args*): KesBinaryTree =
      args.length match {
        case 0 =>
          KesBinaryTree.SigningLeaf(sk.toArray, vk.toArray)
        case _ =>
          if (args.head._1) {
            KesBinaryTree.MerkleNode(
              args.head._2._1.toArray,
              args.head._2._2.toArray,
              args.head._2._3.toArray,
              KesBinaryTree.Empty,
              this.build(sk, vk, args.tail: _*)
            )
          } else {
            KesBinaryTree.MerkleNode(
              args.head._2._1.toArray,
              args.head._2._2.toArray,
              args.head._2._3.toArray,
              this.build(sk, vk, args.tail: _*),
              KesBinaryTree.Empty
            )
          }
      }
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
        Bytes(seed_a) == Bytes(seed_b) &&
          Bytes(witnessLeft_a) == Bytes(witnessLeft_b) &&
          Bytes(witnessRight_a) == Bytes(witnessRight_b) &&
          areEqual(left_a, left_b) &&
          areEqual(right_a, right_b)
      case (KesBinaryTree.Empty, KesBinaryTree.Empty) =>
        true
      case (KesBinaryTree.SigningLeaf(sk_a, vk_a), KesBinaryTree.SigningLeaf(sk_b, vk_b)) =>
        Bytes(sk_a) == Bytes(sk_b) &&
          Bytes(vk_a) == Bytes(vk_b)
      case _ =>
        false
    }

  case class MerkleNode(
    seed:         Array[Byte],
    witnessLeft:  Array[Byte],
    witnessRight: Array[Byte],
    left:         KesBinaryTree,
    right:        KesBinaryTree
  ) extends KesBinaryTree {
    override def equals(obj: Any): Boolean = areEqual(this, obj)
  }

  case class SigningLeaf(sk: Array[Byte], vk: Array[Byte]) extends KesBinaryTree {
    override def equals(obj: Any): Boolean = areEqual(this, obj)
  }

  case object Empty extends KesBinaryTree
}
