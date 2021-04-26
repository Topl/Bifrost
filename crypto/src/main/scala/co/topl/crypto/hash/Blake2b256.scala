package co.topl.crypto.hash

import cats.data.NonEmptyChain
import org.bouncycastle.crypto.digests.Blake2bDigest

case class Blake2b256()

object Blake2b256 {

  private def digestSizeInBits(digestSize: Int) = digestSize * 8

  implicit val digest32: Hash[Blake2b256, Digest32] = new Hash[Blake2b256, Digest32] {

    private lazy val digestFunc = new Blake2bDigest(digestSizeInBits(Digest32.size))

    override def hash(prefix: Option[Byte], messages: NonEmptyChain[Array[Byte]]): Digest32 =
      Digest32(Blake2b256.hash(prefix, messages, digestFunc, Digest32.size))
  }

  implicit val digest64: Hash[Blake2b256, Digest64] = new Hash[Blake2b256, Digest64] {

    private lazy val digestFunc = new Blake2bDigest(digestSizeInBits(Digest64.size))

    override def hash(prefix: Option[Byte], messages: NonEmptyChain[Array[Byte]]): Digest64 =
      Digest64(Blake2b256.hash(prefix, messages, digestFunc, Digest64.size))
  }

  private def hash(
    prefix: Option[Byte],
    messages: NonEmptyChain[Array[Byte]],
    digestFunc: Blake2bDigest,
    digestSize: Int
  ): Array[Byte] =
    // must be synchronized on the digest function so that everyone shares an instance
    synchronized {
      // update digest with prefix and messages
      prefix.foreach(p => digestFunc.update(p))
      messages.iterator.foreach(m => digestFunc.update(m, 0, m.length))

      val res = new Array[Byte](digestSize)

      // calling .doFinal resets to a default state
      digestFunc.doFinal(res, 0)

      res
    }
}
