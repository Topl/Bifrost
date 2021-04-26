package co.topl.crypto.hash

import cats.data.NonEmptyChain
import org.bouncycastle.crypto.digests.Blake2bDigest

case class Blake2b256()

object Blake2b256 {

  /** Blake2b256 hashing function implementation. */
  implicit val hash: Hash[Blake2b256] = new Hash[Blake2b256] {

    override val digestSize = 32

    private val digestSizeInBits = digestSize * 8

    private lazy val blake2b256DigestFunc = new Blake2bDigest(digestSizeInBits)

    override def hash(prefix: Option[Byte], messages: NonEmptyChain[Array[Byte]]): Digest =
      // must be synchronized on the digest function so that everyone shares an instance
      synchronized {
        // update digest with prefix and messages
        prefix.foreach(p => blake2b256DigestFunc.update(p))
        messages.iterator.foreach(m => blake2b256DigestFunc.update(m, 0, m.length))

        val res = new Array[Byte](digestSize)

        // calling .doFinal resets to a default state
        blake2b256DigestFunc.doFinal(res, 0)

        Digest(res)
      }

  }
}
