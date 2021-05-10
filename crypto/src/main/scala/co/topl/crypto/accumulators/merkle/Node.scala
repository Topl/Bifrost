package co.topl.crypto.accumulators.merkle

import co.topl.crypto.accumulators.{EmptyByteArray, LeafData}
import co.topl.crypto.hash.Hash
import co.topl.crypto.hash.digest.Digest

/* Forked from https://github.com/input-output-hk/scrypto */

abstract class Node[D: Digest] {
  def hash: D
}

/**
 * Internal node in Merkle tree
 *
 * @param left  - left child. always non-empty
 * @param right - right child. can be emptyNode
 */
case class InternalNode[H, D: Digest](left: Node[D], right: Node[D])(implicit hashFunc: Hash[H, D]) extends Node[D] {

  override lazy val hash: D =
    hashFunc.hash(MerkleTree.InternalNodePrefix, Digest[D].bytes(left.hash) ++ Digest[D].bytes(right.hash))

  // TODO: This is temporarily disabled because we removed Base58, use Hex.scala in test here if needed
  //  override def toString: String = s"InternalNode(" +
  //    s"left: ${Base58.encode(left.hash)}, " +
  //    s"right: ${if (BytesOf[D].isEmpty(right.hash)) "null" else Base58.encode(right.hash)}," +
  //    s"hash: ${Base58.encode(hash)})"
}

/**
 * Merkle tree leaf
 *
 * @param data - leaf data.
 */
case class Leaf[H, D: Digest](data: LeafData)(implicit h: Hash[H, D]) extends Node[D] {
  override lazy val hash: D = Hash[H, D].hash(MerkleTree.LeafPrefix, data.value)

  // TODO: This is temporarily disabled because we removed Base58, use Hex.scala in test here if needed
  //  override def toString: String = s"Leaf(${Base58.encode(hash)})"
}

/**
 * Empty Merkle tree node.
 * Either Leaf (if number of non-empty leafs is not a power of 2, remaining leafs are EmptyNode)
 * or InternalNode (if both childs of an InternalNode are empty, it is EmptyNode)
 */
case class EmptyNode[H, D: Digest]()(implicit h: Hash[H, D]) extends Node[D] {
  override val hash: D = EmptyByteArray.asInstanceOf[D]
}

/**
 * Empty root node. If the tree contains no elements, it's root hash is array of 0 bits of a hash function digest
 * length
 */
case class EmptyRootNode[H, D: Digest]()(implicit h: Hash[H, D]) extends Node[D] {

  override val hash: D = Digest[D]
    .from(Array.fill(Digest[D].size)(0: Byte))
    .valueOr(ex => throw new Exception(s"Failed to create empty digest: $ex"))

  // TODO: This is temporarily disabled because we removed Base58, use Hex.scala in test here if needed
  //  override def toString: String = s"EmptyRootNode(${Base58.encode(hash)})"
}
