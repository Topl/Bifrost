package co.topl.crypto.accumulators.merkle

import cats.implicits._
import co.topl.crypto.accumulators.LeafData
import co.topl.crypto.hash.digest.Digest
import co.topl.crypto.hash.digest.implicits._
import co.topl.crypto.hash.{Hash, HashFailure, HashResult}

/* Forked from https://github.com/input-output-hk/scrypto */

sealed abstract class Node[D: Digest] {
  def hash: HashResult[D]
}

/**
 * Internal node in Merkle tree
 *
 * @param left  - left child. always non-empty
 * @param right - right child. can be emptyNode
 */
case class InternalNode[H, D: Digest](left: Node[D], right: Option[Node[D]])(implicit hashFunc: Hash[H, D])
    extends Node[D] {

  override lazy val hash: HashResult[D] =
    for {
      leftHashBytes  <- left.hash.map(_.bytes)
      rightHashBytes <- right.map(_.hash.map(_.bytes)).getOrElse(Array.emptyByteArray.asRight[HashFailure])
      nodeHash       <- hashFunc.hash(MerkleTree.InternalNodePrefix, leftHashBytes ++ rightHashBytes)
    } yield nodeHash
}

/**
 * Merkle tree leaf
 *
 * @param data - leaf data.
 */
case class Leaf[H, D: Digest](data: LeafData)(implicit h: Hash[H, D]) extends Node[D] {
  override lazy val hash: HashResult[D] = Hash[H, D].hash(MerkleTree.LeafPrefix, data.value)
}
