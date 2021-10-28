package co.topl.models.utility

sealed trait KesBinaryTree

object KesBinaryTree {

  case class MerkleNode(
    seed:         Array[Byte],
    witnessLeft:  Array[Byte],
    witnessRight: Array[Byte],
    left:         KesBinaryTree,
    right:        KesBinaryTree
  ) extends KesBinaryTree

  case class SigningLeaf(sk: Array[Byte], vk: Array[Byte]) extends KesBinaryTree

  case object Empty extends KesBinaryTree
}
