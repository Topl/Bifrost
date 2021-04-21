package co.topl.crypto.authds.merkle

import co.topl.crypto.authds.{EmptyByteArray, LeafData}
import co.topl.crypto.hash.{Digest, Hash}
import scorex.util.ScorexEncoding

/* Forked from https://github.com/input-output-hk/scrypto */

trait Node extends ScorexEncoding {
  def hash: Digest
}

/** Internal node in Merkle tree
 *
 * @param left  - left child. always non-empty
 * @param right - right child. can be emptyNode
 */
case class InternalNode[H : Hash](left: Node, right: Node) extends Node {

  override lazy val hash: Digest = Hash(MerkleTree.InternalNodePrefix, left.hash, right.hash)

  override def toString: String = s"InternalNode(" +
    s"left: ${encoder.encode(left.hash)}, " +
    s"right: ${if (right.hash.isEmpty) "null" else encoder.encode(right.hash)}," +
    s"hash: ${encoder.encode(hash)})"
}

/** Merkle tree leaf
 *
 * @param data - leaf data.
 */
case class Leaf[H : Hash](data: LeafData) extends Node {
  override lazy val hash: Digest = Hash(MerkleTree.LeafPrefix, data)

  override def toString: String = s"Leaf(${encoder.encode(hash)})"
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
  // .get is secure here since we know that array size equals to digest size
  override val hash: Digest = Hash.byteArrayToDigest(Array.fill(Hash.digestSize)(0: Byte)).get

  override def toString: String = s"EmptyRootNode(${encoder.encode(hash)})"
}
