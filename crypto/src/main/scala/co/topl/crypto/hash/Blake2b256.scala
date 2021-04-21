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

    override def hash(input: Array[Byte]): Digest = {
      Digest @@ synchronized {
        digestFn.update(input, 0, input.length)
        val res = new Array[Byte](digestSize)
        digestFn.doFinal(res, 0)
        res
      }
    }

    override def hashWithPrefix(prefix: Byte, inputs: Array[Byte]*): Digest = {
      Digest @@ synchronized {
        digestFn.update(prefix)
        inputs.foreach(i => digestFn.update(i, 0, i.length))
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
