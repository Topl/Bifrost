package co.topl.crypto.accumulators.merkle

import cats.implicits._
import co.topl.crypto.Hex
import co.topl.crypto.accumulators.{LeafData, Side}
import co.topl.crypto.hash.Hash
import co.topl.crypto.hash.digest.Digest
import co.topl.crypto.hash.digest.implicits._

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
case class MerkleProof[H, D: Digest](leafData: LeafData, levels: Seq[(Option[D], Side)])(implicit
  hashFunc:                                    Hash[H, D]
) {

  def valid(expectedRootHash: D): Boolean = {
    val leafHash = hashFunc.hash(MerkleTree.LeafPrefix, leafData.value)

    val result = levels.foldLeft(leafHash) {
      case (Right(prevHash), (hash, side)) =>
        val nodeBytes =
          hash.map { h =>
            if (side == MerkleProof.LeftSide) prevHash.bytes ++ h.bytes
            else h.bytes ++ prevHash.bytes
          } getOrElse prevHash.bytes

        val resultHash = hashFunc.hash(MerkleTree.InternalNodePrefix, nodeBytes)

        println(resultHash.map(r => Hex.encode(r.bytes)).getOrElse("fail"))

        resultHash

      case (invalidHash, _) => invalidHash
    }

    val isValid = result.map(r => r === expectedRootHash).getOrElse(false)

    isValid
  }

  override def toString: String =
    s"MerkleProof(${Hex.encode(leafData.value)}, levels: ${levels.map(l => (l._1.map(d => Hex.encode(d.bytes)), l._2))})"
}

object MerkleProof {

  val LeftSide: Side = Side(0.toByte)
  val RightSide: Side = Side(1.toByte)
}
