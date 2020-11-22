package co.topl.modifier.block

import co.topl.utils.serialization.{BifrostSerializer, BytesSerializable, Reader, Writer}
import scorex.crypto.hash.Blake2b256

import scala.collection.BitSet

/**
  * This implementation of Bloom filter is specced in the Ethereum Yellow Paper
  * for more information, visit: http://gavwood.com/paper.pdf
  */
case class BloomFilter(topics: BitSet) extends BytesSerializable {

  override type M = BloomFilter
  lazy val serializer: BifrostSerializer[BloomFilter] = BloomFilter

}

object BloomFilter extends BifrostSerializer[BloomFilter] {
  def apply(origin: Array[Byte], topics: IndexedSeq[Array[Byte]]): BitSet = {
    // taking the low-order 11 bits (mod 2048) of each of the first three pairs of bytes in a Keccak-256 hash
    val indices = (origin +: topics).flatMap { x =>
      val hash = Blake2b256(x)
      List(0, 2, 4).map ( i =>
        // Need to convert signed Byte to unsigned Int
        hash.slice(i, i + 2).map(_ + 128).sum % 2048
      )
    }
    BitSet() ++ indices
  }

  override def serialize(obj: BloomFilter, w: Writer): Unit = {
    w.putInt(obj.topics.size)
    obj.topics.foreach(t => w.putInt(t))
  }

  override def parse(r: Reader): BloomFilter = {
    val topicsLength = r.getInt()
    val topics: BitSet = BitSet((for (_ <- 0 until topicsLength) yield r.getInt()): _*)
    BloomFilter(topics)
  }
}

