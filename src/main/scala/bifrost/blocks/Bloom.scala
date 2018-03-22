package bifrost.blocks

import scorex.crypto.hash.Keccak256

import scala.collection.BitSet

/**
  * This implementation of Bloom filter is specced in the Ethereum Yellow Paper
  * for more information, visit: http://gavwood.com/paper.pdf
  */
object Bloom {
  def calcBloom(origin: Array[Byte], topics: IndexedSeq[Array[Byte]]): BitSet = {
    // taking the low-order 11 bits (mod 2048) of each of the first three pairs of bytes in a Keccak-256 hash
    val indices = (origin +: topics).flatMap { x =>
      val hash = Keccak256(x)
      //noinspection ScalaStyle
      List(0, 2, 4).map ( i =>
        // Need to convert signed Byte to unsigned Int
        hash.slice(i, i + 2).map(_ + 128).sum % 2048
      )
    }
    BitSet() ++ indices
  }
}