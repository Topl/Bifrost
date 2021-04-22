package co.topl.crypto.authds.merkle

import co.topl.crypto.authds.{EmptyByteArray, LeafData}
import co.topl.crypto.hash.{Digest, Hash}
import co.topl.crypto.utils.Base16

/* Forked from https://github.com/input-output-hk/scrypto */

trait Node {
  def hash: Digest
}

/** Internal node in Merkle tree
 *
 * @param left  - left child. always non-empty
 * @param right - right child. can be emptyNode
 */
case class InternalNode[H : Hash](left: Node, right: Node) extends Node {

  override lazy val hash: Digest = Hash(MerkleTree.InternalNodePrefix, left.hash.bytes ++ right.hash.bytes)

  override def toString: String = s"InternalNode(" +
    s"left: ${Base16.encode(left.hash.bytes)}, " +
    s"right: ${if (right.hash.bytes.isEmpty) "null" else Base16.encode(right.hash.bytes)}," +
    s"hash: ${Base16.encode(hash.bytes)})"
}

/** Merkle tree leaf
 *
 * @param data - leaf data.
 */
case class Leaf[H : Hash](data: LeafData) extends Node {
  override lazy val hash: Digest = Hash(MerkleTree.LeafPrefix, data)

  override def toString: String = s"Leaf(${Base16.encode(hash.bytes)})"
}

/** Empty Merkle tree node.
 * Either Leaf (if number of non-empty leafs is not a power of 2, remaining leafs are EmptyNode)
 * or InternalNode (if both childs of an InternalNode are empty, it is EmptyNode)
 */
case class EmptyNode[H : Hash]() extends Node {
  override val hash: Digest = EmptyByteArray.asInstanceOf[Digest]
}

/** Empty root node. If the tree contains no elements, it's root hash is array of 0 bits of a hash function digest
 * length
 */
case class EmptyRootNode[H : Hash]() extends Node {
  override val hash: Digest = Digest(Array.fill(Hash.digestSize)(0: Byte))

  override def toString: String = s"EmptyRootNode(${Base16.encode(hash.bytes)})"
}
