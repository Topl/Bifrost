package co.topl.crypto.accumulators.merkle

import co.topl.crypto.accumulators.{LeafData, Side}
import co.topl.crypto.hash.Hash
import co.topl.crypto.hash.digest.Digest
import co.topl.crypto.hash.digest.implicits._

import scala.annotation.tailrec
import scala.collection.mutable

/* Forked from https://github.com/input-output-hk/scrypto */

case class MerkleTree[H, D: Digest](
  topNode:           Node[D],
  elementsHashIndex: Map[mutable.WrappedArray.ofByte, Int]
)(implicit h:        Hash[H, D]) {

  lazy val rootHash: D = topNode.hash
  lazy val length: Int = elementsHashIndex.size

  def proofByElement(element: Leaf[H, D]): Option[MerkleProof[H, D]] = proofByElementHash(element.hash)

  def proofByElementHash(hash: D): Option[MerkleProof[H, D]] =
    elementsHashIndex.get(new mutable.WrappedArray.ofByte(hash.bytes)).flatMap(i => proofByIndex(i))

  def proofByIndex(index: Int): Option[MerkleProof[H, D]] = if (index >= 0 && index < length) {

    @tailrec
    def loop(
      node:      Node[D],
      i:         Int,
      curLength: Int,
      acc:       Seq[(D, Side)]
    ): Option[(Leaf[H, D], Seq[(D, Side)])] =
      node match {
        case n: InternalNode[H, D] if i < curLength / 2 =>
          loop(n.left, i, curLength / 2, acc :+ (n.right.hash, MerkleProof.LeftSide))
        case n: InternalNode[H, D] if i < curLength =>
          loop(n.right, i - curLength / 2, curLength / 2, acc :+ (n.left.hash, MerkleProof.RightSide))
        case n: Leaf[H, D] =>
          Some((n, acc.reverse))
        case _ =>
          None
      }

    val leafWithProofs = loop(topNode, index, lengthWithEmptyLeafs, Seq())
    leafWithProofs.map(lp => MerkleProof(lp._1.data, lp._2))
  } else {
    None
  }

  lazy val lengthWithEmptyLeafs: Int = {
    def log2(x: Double): Double = math.log(x) / math.log(2)

    Math.max(math.pow(2, math.ceil(log2(length))).toInt, 2)
  }

  // TODO: This is temporarily disabled because we removed Base58, use Hex.scala in test here if needed
  //  //Debug only
  //  override lazy val toString: String = {
  //
  //    @tailrec
  //    def loop(nodes: Seq[Node[D]], level: Int, acc: String): String =
  //      if (nodes.nonEmpty) {
  //        val thisLevStr = s"Level $level: " + nodes.map(_.toString).mkString(",") + "\n"
  //        val nextLevNodes = nodes.flatMap {
  //          case i: InternalNode[H, D] => Seq(i.left, i.right)
  //          case _                     => Seq()
  //        }
  //        loop(nextLevNodes, level + 1, acc + thisLevStr)
  //      } else {
  //        acc
  //      }
  //
  //    loop(Seq(topNode), 0, "")
  //  }
}

object MerkleTree {
  val LeafPrefix: Byte = 0: Byte
  val InternalNodePrefix: Byte = 1: Byte

  /**
   * Construct Merkle tree from leafs
   *
   * @param payload       - sequence of leafs data
   * @return MerkleTree constructed from current leafs with defined empty node and hash function
   */
  def apply[H, D: Digest](payload: Seq[LeafData])(implicit h: Hash[H, D]): MerkleTree[H, D] = {
    val leafs = payload.map(d => Leaf[H, D](d))
    val elementsIndex: Map[mutable.WrappedArray.ofByte, Int] = leafs.indices.map { i =>
      (new mutable.WrappedArray.ofByte(leafs(i).hash.bytes), i)
    }.toMap
    val topNode = calcTopNode[H, D](leafs)

    MerkleTree[H, D](topNode, elementsIndex)
  }

  @tailrec
  def calcTopNode[H, D: Digest](nodes: Seq[Node[D]])(implicit h: Hash[H, D]): Node[D] =
    if (nodes.isEmpty) {
      EmptyRootNode[H, D]
    } else {
      val nextNodes = nodes
        .grouped(2)
        .map(lr => InternalNode[H, D](lr.head, if (lr.lengthCompare(2) == 0) lr.last else EmptyNode[H, D]))
        .toSeq
      if (nextNodes.lengthCompare(1) == 0) nextNodes.head else calcTopNode(nextNodes)
    }
}
