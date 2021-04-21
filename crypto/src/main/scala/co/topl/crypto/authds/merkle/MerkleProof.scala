package co.topl.crypto.authds.merkle

import co.topl.crypto.authds.{LeafData, Side}
import co.topl.crypto.hash.{CryptographicHash, Digest}
import co.topl.crypto.utils.encode.Base16

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
                                   (implicit val hf: CryptographicHash[D]) extends {

  def valid(expectedRootHash: Digest): Boolean = {
    val leafHash = hf.prefixedHash(MerkleTree.LeafPrefix, leafData)

    levels.foldLeft(leafHash) { case (prevHash, (hash, side)) =>
      if (side == MerkleProof.LeftSide) {
        hf.prefixedHash(MerkleTree.InternalNodePrefix, prevHash ++ hash)
      } else {
        hf.prefixedHash(MerkleTree.InternalNodePrefix, hash ++ prevHash)
      }
    }.sameElements(expectedRootHash)
  }

  override def toString: String =
    s"MerkleProof(data: ${Base16.encode(leafData)}, hash: ${Base16.encode(hf(leafData))}, " +
      s"(${levels.map(ht => Base16.encode(ht._1) + " : " + ht._2)}))"
}

object MerkleProof {

  val LeftSide: Side = Side @@ 0.toByte
  val RightSide: Side = Side @@ 1.toByte
}

