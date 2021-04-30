package co.topl.crypto.hash

import org.bouncycastle.crypto.digests.Blake2bDigest

case class Blake2b()

object Blake2b {

  def blake2bHashFor[D: Digest]: Hash[Blake2b, D] = new Hash[Blake2b, D] {
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

        Digest[D].from(res)
      }
  }

  implicit val blake2b256: Hash[Blake2b, Digest32] = blake2bHashFor[Digest32]

  implicit val blake2b512: Hash[Blake2b, Digest64] = blake2bHashFor[Digest64]

}
