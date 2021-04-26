package co.topl.crypto.accumulators.merkle

import co.topl.crypto.accumulators.{EmptyByteArray, LeafData}
import co.topl.crypto.hash.{Hash, Digest32}
import co.topl.crypto.utils.Base58

/* Forked from https://github.com/input-output-hk/scrypto */

trait Node {
  def hash: Digest32
}

/** Internal node in Merkle tree
 *
 * @param left  - left child. always non-empty
 * @param right - right child. can be emptyNode
 */
case class InternalNode[H](left: Node, right: Node)(implicit h: Hash[H, Digest32]) extends Node {

  override lazy val hash: Digest32 = Hash[H, Digest32](
    MerkleTree.InternalNodePrefix,
    left.hash.toBytes ++ right.hash.toBytes)

  override def toString: String = s"InternalNode(" +
    s"left: ${Base58.encode(left.hash.toBytes)}, " +
    s"right: ${if (right.hash.toBytes.isEmpty) "null" else Base58.encode(right.hash.toBytes)}," +
    s"hash: ${Base58.encode(hash.toBytes)})"
}

/** Merkle tree leaf
 *
 * @param data - leaf data.
 */
case class Leaf[H](data: LeafData)(implicit h: Hash[H, Digest32]) extends Node {
  override lazy val hash: Digest32 = Hash[H, Digest32](MerkleTree.LeafPrefix, data.toBytes)

  override def toString: String = s"Leaf(${Base58.encode(hash.toBytes)})"
}

/** Empty Merkle tree node.
 * Either Leaf (if number of non-empty leafs is not a power of 2, remaining leafs are EmptyNode)
 * or InternalNode (if both childs of an InternalNode are empty, it is EmptyNode)
 */
case class EmptyNode[H](implicit h: Hash[H, Digest32]) extends Node {
  override val hash: Digest32 = EmptyByteArray.asInstanceOf[Digest32]
}

/** Empty root node. If the tree contains no elements, it's root hash is array of 0 bits of a hash function digest
 * length
 */
case class EmptyRootNode[H](implicit h: Hash[H, Digest32]) extends Node {
  // .get is secure here since we know that array size equals to digest size
  override val hash: Digest32 = Digest32(Array.fill(Hash[H, Digest32].digestSize)(0: Byte))

  override def toString: String = s"EmptyRootNode(${Base58.encode(hash.toBytes)})"
}
