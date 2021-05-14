package co.topl.crypto.accumulators.merkle

import cats.implicits._
import co.topl.crypto.accumulators.{LeafData, Side}
import co.topl.crypto.hash.digest.Digest
import co.topl.crypto.hash.digest.implicits._
import co.topl.crypto.hash.{Hash, HashFailure, HashResult, InvalidDigestFailure}

import scala.annotation.tailrec
import scala.collection.mutable

/* Forked from https://github.com/input-output-hk/scrypto */

case class MerkleTree[H, D: Digest](
  topNode:           Option[Node[D]],
  elementsHashIndex: Map[mutable.WrappedArray.ofByte, Int]
)(implicit h:        Hash[H, D]) {

  private lazy val emptyRootHash: HashResult[D] =
    Digest[D]
      .from(Array.fill(Digest[D].size)(0: Byte))
      .leftMap(InvalidDigestFailure)
      .toEither

  lazy val rootHash: HashResult[D] =
    topNode
      .map(_.hash)
      .getOrElse(emptyRootHash)

  lazy val length: Int = elementsHashIndex.size

  def proofByElement(element: Leaf[H, D]): Option[MerkleProof[H, D]] =
    element.hash.toOption.flatMap(proofByElementHash)

  def proofByElementHash(hash: D): Option[MerkleProof[H, D]] =
    elementsHashIndex.get(new mutable.WrappedArray.ofByte(hash.bytes)).flatMap(i => proofByIndex(i))

  def proofByIndex(index: Int): Option[MerkleProof[H, D]] = if (index >= 0 && index < length) {

    @tailrec
    def loop(
      node:      Option[Node[D]],
      i:         Int,
      curLength: Int,
      acc:       Seq[(Option[D], Side)]
    ): Option[(Leaf[H, D], Seq[(Option[D], Side)])] =
      node match {
        case Some(n: InternalNode[H, D]) if i < curLength / 2 =>
          n.right match {
            case Some(right) =>
              right.hash match {
                case Right(hash) => loop(Some(n.left), i, curLength / 2, acc :+ (Some(hash), MerkleProof.LeftSide))
                case Left(_)     => None
              }
            case None => loop(Some(n.left), i, curLength / 2, acc :+ (None, MerkleProof.LeftSide))
          }
        case Some(n: InternalNode[H, D]) if i < curLength =>
          n.hash match {
            case Right(hash) =>
              loop(n.right, i - curLength / 2, curLength / 2, acc :+ (Some(hash), MerkleProof.RightSide))
            case Left(_) => None
          }
        case Some(n: Leaf[H, D]) =>
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

  sealed trait MerkleTreeFailure
  case class LeafHashFailure(failure: HashFailure) extends MerkleTreeFailure

  type MerkleTreeResult[H, D] = Either[MerkleTreeFailure, MerkleTree[H, D]]

  /**
   * Construct Merkle tree from leafs
   *
   * @param payload       - sequence of leafs data
   * @return MerkleTree constructed from current leafs with defined empty node and hash function
   */
  def construct[H, D: Digest](payload: Seq[LeafData])(implicit h: Hash[H, D]): MerkleTreeResult[H, D] = {
    val leafs = payload.map(d => Leaf[H, D](d))

    val elementsToIndex =
      leafs.zipWithIndex
        .foldLeft(Map[mutable.WrappedArray.ofByte, Int]().asRight[MerkleTreeFailure]) {
          case (elements, (leaf, leafIndex)) =>
            elements.flatMap(e =>
              leaf.hash
                .leftMap(LeafHashFailure)
                .map(h => new mutable.WrappedArray.ofByte(h.bytes))
                .map(w => e + (w -> leafIndex))
            )
        }

    val topNode = calcTopNode[H, D](leafs)

    elementsToIndex.map(MerkleTree[H, D](topNode, _))
  }

  @tailrec
  def calcTopNode[H, D: Digest](nodes: Seq[Node[D]])(implicit h: Hash[H, D]): Option[Node[D]] =
    if (nodes.isEmpty) {
      None
    } else {
      val nextNodes = nodes
        .grouped(2)
        .map(lr => InternalNode[H, D](lr.head, if (lr.lengthCompare(2) == 0) Some(lr.last) else None))
        .toSeq
      if (nextNodes.lengthCompare(1) == 0) Some(nextNodes.head) else calcTopNode(nextNodes)
    }
}
