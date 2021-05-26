package co.topl.crypto.hash

import co.topl.crypto.hash.digest.implicits._
import co.topl.crypto.hash.digest.{Digest, Digest64}
import org.bouncycastle.crypto.macs.HMac
import org.bouncycastle.crypto.digests.SHA512Digest

object HmacSha512 {
  val hmacSha = new HMac(new SHA512Digest())
}

class HmacSha512 extends Hash[HmacSha, Digest64] {

  private lazy val hmacShaFunc = new HMac(new SHA512Digest())

  /**
   * Hashes a set of messages with an optional prefix.
   *
   * @param prefix   the optional prefix byte of the hashed message
   * @param messages the set of messages to iteratively hash
   * @return the hash digest
   */
  override def hash(prefix: Option[Byte], messages: Message*): Digest64 = synchronized {
    prefix.foreach(p => hmacShaFunc.update(p))
    messages.iterator.foreach { m =>
      hmacShaFunc.update(m, 0, m.length)
    }

    val res = new Array[Byte](Digest64.size)

    // calling .doFinal resets to a default state
    hmacShaFunc.doFinal(res, 0)

    Digest64
      .validated(res)
      .valueOr(err => throw new Error(s"Hmac Sha hash with digest size ${Digest64.size} was invalid! $err"))
  }
}
