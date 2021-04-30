package co.topl.crypto.accumulators.merkle

import co.topl.crypto.accumulators.{LeafData, Side}
import co.topl.crypto.hash.{Digest, Hash}

/* Forked from https://github.com/input-output-hk/scrypto */

/**
 * Proof is given leaf data, leaf hash sibling and also siblings for parent nodes. Using this data, it is possible to
 * compute nodes on the path to root hash, and the hash itself. The picture of a proof given below. In the picture,
 * "^^" is leaf data(to compute leaf hash from), "=" values are to be computed, "*" values are to be stored.
 *
 * ........= Root
 * ..... /  \
 * .... *   =
 * ....... / \
 * ...... *   =
 * ......... /.\
 * .........*   =
 * ............ ^^
 *
 * @param leafData - leaf data bytes
 * @param levels - levels in proof, bottom up, each level is about stored value and position of computed element
 *               (whether it is left or right to stored value)
 */
case class MerkleProof[H, D: Digest](leafData: LeafData, levels: Seq[(D, Side)])(implicit
  hashFunc:                                             Hash[H, D]
) {

  def valid(expectedRootHash: D): Boolean = {
    val leafHash = hashFunc.hash(MerkleTree.LeafPrefix, leafData.value)

    val result = levels.foldLeft(leafHash) { case (prevHash, (hash, side)) =>
      if (side == MerkleProof.LeftSide) {
        hashFunc.hash(MerkleTree.InternalNodePrefix, Digest[D].concat(prevHash, hash))
      } else {
        hashFunc.hash(MerkleTree.InternalNodePrefix, Digest[D].concat(hash, prevHash))
      }
    }

    Digest[D].sameElements(result, expectedRootHash)
  }

  // TODO: This is temporarily disabled because we removed Base58, use Hex.scala in test here if needed
  //  override def toString: String =
  //    s"MerkleProof(data: ${Base58.encode(leafData)}, hash: ${Base58.encode(hashFunc.hash(leafData))}, " +
  //    s"(${levels.map(ht => Base58.encode(ht._1) + " : " + ht._2)}))"
}

object MerkleProof {

  val LeftSide: Side = Side(0.toByte)
  val RightSide: Side = Side(1.toByte)
}
