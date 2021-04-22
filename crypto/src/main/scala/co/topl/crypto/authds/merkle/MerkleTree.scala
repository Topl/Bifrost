package co.topl.crypto.authds.merkle

import co.topl.crypto.authds.{LeafData, Side}
import co.topl.crypto.hash.{Digest, Hash}

import scala.annotation.tailrec
import scala.collection.mutable

/* Forked from https://github.com/input-output-hk/scrypto */

case class MerkleTree[H : Hash](topNode: Node,
                                   elementsHashIndex: Map[mutable.WrappedArray.ofByte, Int]) {

  lazy val rootHash: Digest = topNode.hash
  lazy val length: Int = elementsHashIndex.size

  def proofByElement(element: Leaf[H]): Option[MerkleProof[H]] = proofByElementHash(element.hash)

  def proofByElementHash(hash: Digest): Option[MerkleProof[H]] = {
    elementsHashIndex.get(new mutable.WrappedArray.ofByte(hash.bytes)).flatMap(i => proofByIndex(i))
  }

  def proofByIndex(index: Int): Option[MerkleProof[H]] = if (index >= 0 && index < length) {

    @tailrec
    def loop(
      node: Node,
      i: Int,
      curLength: Int,
      acc: Seq[(Digest, Side)]
    ): Option[(Leaf[H], Seq[(Digest, Side)])] = {
      node match {
        case n: InternalNode[H] if i < curLength / 2 =>
          loop(n.left, i, curLength / 2, acc :+ (n.right.hash, MerkleProof.LeftSide))
        case n: InternalNode[H] if i < curLength =>
          loop(n.right, i - curLength / 2, curLength / 2, acc :+ (n.left.hash, MerkleProof.RightSide))
        case n: Leaf[H] =>
          Some((n, acc.reverse))
        case _ =>
          None
      }
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

  //Debug only
  override lazy val toString: String = {

    @tailrec
    def loop(nodes: Seq[Node], level: Int, acc: String): String = {
      if (nodes.nonEmpty) {
        val thisLevStr = s"Level $level: " + nodes.map(_.toString).mkString(",") + "\n"
        val nextLevNodes = nodes.flatMap {
          case i: InternalNode[H] => Seq(i.left, i.right)
          case _ => Seq()
        }
        loop(nextLevNodes, level + 1, acc + thisLevStr)
      } else {
        acc
      }
    }

    loop(Seq(topNode), 0, "")
  }
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
  def apply[H : Hash](payload: Seq[LeafData]): MerkleTree[H] = {
    val leafs = payload.map(d => Leaf(d))
    val elementsIndex: Map[mutable.WrappedArray.ofByte, Int] = leafs.indices.map { i =>
      (new mutable.WrappedArray.ofByte(leafs(i).hash.bytes), i)
    }.toMap
    val topNode = calcTopNode(leafs)

    MerkleTree(topNode, elementsIndex)
  }

  @tailrec
  def calcTopNode[H : Hash](nodes: Seq[Node]): Node = {
    if (nodes.isEmpty) {
      EmptyRootNode[H]
    } else {
      val nextNodes = nodes.grouped(2)
        .map(lr => InternalNode[H](lr.head, if (lr.lengthCompare(2) == 0) lr.last else EmptyNode[H])).toSeq
      if (nextNodes.lengthCompare(1) == 0) nextNodes.head else calcTopNode(nextNodes)
    }
  }
}
