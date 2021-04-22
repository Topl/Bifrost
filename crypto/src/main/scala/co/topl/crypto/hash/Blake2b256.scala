package co.topl.crypto.hash

import cats.implicits._
import cats.data.NonEmptyChain
import org.bouncycastle.crypto.digests.Blake2bDigest

import scala.util.Try

case class Blake2b256()

object Blake2b256 {

  /** Blake2b256 hashing function implementation. */
  implicit val hash: Hash[Blake2b256] = new Hash[Blake2b256] {

    private val blake2b256DigestSize = 256

    override val digestSize = 32

    override def hash(prefix: Option[Byte], messages: NonEmptyChain[Array[Byte]]): Digest = {
      val blake2bDigest = new Blake2bDigest(blake2b256DigestSize)
      prefix.foreach(p => blake2bDigest.update(p))

      messages.iterator.foreach(m => blake2bDigest.update(m, 0, m.length))

      val res = new Array[Byte](digestSize)
      blake2bDigest.doFinal(res, 0)

      Digest(res)
    }

  }
}
