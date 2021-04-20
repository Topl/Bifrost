package co.topl.crypto.authds.merkle

import co.topl.crypto.authds.{LeafData, Side}
import co.topl.crypto.hash.{Digest, Hash}
import scorex.util.ScorexEncoding

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
case class MerkleProof[D <: Digest](leafData: LeafData, levels: Seq[(Digest, Side)])
                                   (implicit val hashFunc: Hash[D]) extends ScorexEncoding {

  def valid(expectedRootHash: Digest): Boolean = {
    val leafHash = Hash(MerkleTree.LeafPrefix, leafData)

    levels.foldLeft(leafHash) { case (prevHash, (hash, side)) =>
      if (side == MerkleProof.LeftSide) {
        Hash(MerkleTree.InternalNodePrefix, prevHash ++ hash)
      } else {
        Hash(MerkleTree.InternalNodePrefix, hash ++ prevHash)
      }
    }.sameElements(expectedRootHash)
  }

  override def toString: String =
    s"MerkleProof(data: ${encoder.encode(leafData)}, hash: ${encoder.encode(Hash(leafData))}, " +
      s"(${levels.map(ht => encoder.encode(ht._1) + " : " + ht._2)}))"
}

object MerkleProof {

  val LeftSide: Side = Side @@ 0.toByte
  val RightSide: Side = Side @@ 1.toByte
}

