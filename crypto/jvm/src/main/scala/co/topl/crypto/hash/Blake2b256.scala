package co.topl.crypto.hash

import org.bouncycastle.crypto.digests.Blake2bDigest
import co.topl.models.Bytes
import co.topl.models.utility.HasLength.instances.bytesLength
import co.topl.models.utility.{Lengths, Sized}

/**
 * A thread-unsafe version of the blake2b interface defined above
 */
class Blake2b256 {
  private val digest = new Blake2bDigest(256)

  def hash(bytes: Bytes*): Sized.Strict[Bytes, Lengths.`32`.type] = {
    val out = new Array[Byte](32)
    bytes.foreach(b => digest.update(b.toArray, 0, b.length.toInt))
    digest.doFinal(out, 0)
    Sized.strictUnsafe(Bytes(out))
  }
}

/**
 * A thread-unsafe version of the blake2b interface defined above
 */
class Blake2b512 {
  private val digest = new Blake2bDigest(512)

  def hash(bytes: Bytes*): Sized.Strict[Bytes, Lengths.`64`.type] = {
    val out = new Array[Byte](64)
    bytes.foreach(b => digest.update(b.toArray, 0, b.length.toInt))
    digest.doFinal(out, 0)
    Sized.strictUnsafe(Bytes(out))
  }
}
