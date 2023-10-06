package co.topl.crypto.models

sealed trait KesBinaryTree

object KesBinaryTree {
  val nodeTypePrefix: Byte = 0
  val leafTypePrefix: Byte = 1
  val emptyTypePrefix: Byte = 2

  final case class MerkleNode(
    seed:         Array[Byte],
    witnessLeft:  Array[Byte],
    witnessRight: Array[Byte],
    left:         KesBinaryTree,
    right:        KesBinaryTree
  ) extends KesBinaryTree {

    override def equals(obj: Any): Boolean =
      obj match {
        case m: MerkleNode =>
          java.util.Arrays.equals(seed, m.seed) &&
          java.util.Arrays.equals(witnessLeft, m.witnessLeft) &&
          java.util.Arrays.equals(witnessRight, m.witnessRight) &&
          left == m.left &&
          right == m.right
        case _ =>
          false
      }

    override def hashCode(): Int = {
      var r = 1
      r = 31 * r + java.util.Arrays.hashCode(seed)
      r = 31 * r + java.util.Arrays.hashCode(witnessLeft)
      r = 31 * r + java.util.Arrays.hashCode(witnessRight)
      r = 31 * r + left.hashCode()
      r = 31 * r + right.hashCode()
      r
    }
  }

  final case class SigningLeaf(sk: Array[Byte], vk: Array[Byte]) extends KesBinaryTree {

    override def equals(obj: Any): Boolean =
      obj match {
        case s: SigningLeaf =>
          java.util.Arrays.equals(sk, s.sk) &&
          java.util.Arrays.equals(vk, s.vk)
        case _ =>
          false
      }

    override def hashCode(): Int = {
      var r = 1
      r = r * 31 + java.util.Arrays.hashCode(sk)
      r = r * 31 + java.util.Arrays.hashCode(vk)
      r
    }
  }

  final case class Empty() extends KesBinaryTree
}
