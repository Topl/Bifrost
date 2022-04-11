package co.topl.models.utility

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
  ) extends KesBinaryTree

  final case class SigningLeaf(sk: Array[Byte], vk: Array[Byte]) extends KesBinaryTree

  final case class Empty() extends KesBinaryTree
}
