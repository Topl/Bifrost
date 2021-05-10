package co.topl.crypto.hash

import co.topl.crypto.hash.digest._
import co.topl.crypto.hash.implicits._
import org.bouncycastle.crypto.digests.Blake2bDigest

abstract class Blake2bHash[D: Digest] extends Hash[Blake2b, D] {
  val digestSize: Int = Digest[D].size
  val digestSizeInBits: Int = 8 * digestSize
  lazy val digestFunc = new Blake2bDigest(digestSizeInBits)

  override def hash(prefix: Option[Byte], messages: Array[Byte]*): D =
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

      Digest[D].from(res).valueOr(ex => throw new Exception(s"Unexpected hash result: $ex"))
    }
}

case object Blake2b256 extends Blake2bHash[Digest32]
case object Blake2b512 extends Blake2bHash[Digest64]
