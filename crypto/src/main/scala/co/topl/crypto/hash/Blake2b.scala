package co.topl.crypto.hash

import co.topl.crypto.hash.digest.Digest
import co.topl.models.Bytes
import co.topl.models.utility.HasLength.instances.bytesLength
import co.topl.models.utility.{Lengths, Sized}
import org.bouncycastle.crypto.digests.Blake2bDigest

abstract class Blake2bHash[D: Digest] extends Hash[Blake2b, D] {
  val digestSize: Int = Digest[D].size
  val digestSizeInBits: Int = 8 * digestSize
  lazy val digestFunc = new Blake2bDigest(digestSizeInBits)

  override def hash(prefix: Option[Byte], messages: Message*): D =
    // must be synchronized on the digest function so that everyone shares an instance
    synchronized {
      // update digest with prefix and messages
      prefix.foreach(p => digestFunc.update(p))
      messages.iterator.foreach { m =>
        digestFunc.update(m, 0, m.length)
      }

      val res = new Array[Byte](digestSize)

      // calling .doFinal resets to a default state
      digestFunc.doFinal(res, 0)

      Digest[D]
        .from(res)
        .valueOr(err => throw new Error(s"Blake2b hash with digest size $digestSize was invalid! $err"))
    }
}

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
