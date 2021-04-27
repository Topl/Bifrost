package co.topl.crypto.accumulators.merkle

import co.topl.crypto.accumulators.{LeafData, Side}
import co.topl.crypto.hash.{Digest32, Hash}
import co.topl.crypto.utils.Base58

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
case class MerkleProof[H](leafData: LeafData, levels: Seq[(Digest32, Side)])(implicit h: Hash[H, Digest32]) {

  def valid(expectedRootHash: Digest32): Boolean = {
    val leafHash = Hash(MerkleTree.LeafPrefix, leafData.value)

    levels.foldLeft(leafHash) { case (prevHash, (hash, side)) =>
      if (side == MerkleProof.LeftSide) {
        Hash(MerkleTree.InternalNodePrefix, prevHash.value ++ hash.value)
      } else {
        Hash(MerkleTree.InternalNodePrefix, hash.value ++ prevHash.value)
      }
    }.value.sameElements(expectedRootHash.value)
  }

  override def toString: String =
    s"MerkleProof(data: ${Base58.encode(leafData.value)}, hash: ${Base58.encode(Hash(leafData.value).value)}, " +
      s"(${levels.map(ht => Base58.encode(ht._1.value) + " : " + ht._2)}))"
}

object MerkleProof {

  val LeftSide: Side = Side(0.toByte)
  val RightSide: Side = Side(1.toByte)
}

