package co.topl.crypto

import scorex.crypto.authds.merkle.{MerkleTree, Node}
import scorex.crypto.hash.Digest32

import scala.collection.mutable

class MerkleTreeBlake2b256(topNode: Node[Digest32], elementsHashIndex: Map[mutable.WrappedArray.ofByte, Int])
  extends MerkleTree[Digest32](topNode, elementsHashIndex) {

}
