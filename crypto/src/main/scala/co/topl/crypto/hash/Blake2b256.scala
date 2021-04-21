package co.topl.crypto.hash

import org.bouncycastle.crypto.digests.Blake2bDigest

import scala.util.Try

case class Blake2b256()

object Blake2b256 {

  /** Blake2b256 hashing function implementation. */
  implicit val hash: Hash[Blake2b256] = new Hash[Blake2b256] {

    private val blake2b256DigestSize = 256

    override val digestSize = 32
    lazy val digestFn: Blake2bDigest = new Blake2bDigest(blake2b256DigestSize)

    override def hash(message: Array[Byte]): Digest = {
      Digest @@ synchronized {
        digestFn.update(message, 0, message.length)
        val res = new Array[Byte](digestSize)
        digestFn.doFinal(res, 0)
        res
      }
    }

    override def hashWithPrefix(prefix: Byte, message: Array[Byte]): Digest = {
      Digest @@ synchronized {
        digestFn.update(prefix)
        digestFn.update(message, 0, message.length)
        val res = new Array[Byte](digestSize)
        digestFn.doFinal(res, 0)
        res
      }
    }

    override def byteArrayToDigest(bytes: Array[Byte]): Try[Digest] = Try {
      require(bytes.lengthCompare(digestSize) == 0, "Incorrect digest size")
      Digest @@ bytes
    }

  }
}
