package co.topl.crypto.hash

import co.topl.crypto.BytesOf
import co.topl.crypto.Implicits._

import java.security.MessageDigest

case class Sha()

object Sha {

  def shaHashFor[D: Digest](algorithm: String): Hash[Sha, D] = new Hash[Sha, D] {

    override def hash[M: BytesOf](prefix: Option[Byte], messages: M*): D =
      Digest[D].from(
        MessageDigest
          .getInstance(algorithm)
          .digest(
            messages.foldLeft(
              prefix
                .map(Array[Byte](_))
                .getOrElse(Array[Byte]())
            )(BytesOf[Array[Byte]].concat)
          )
      )

  }

  implicit val sha256: Hash[Sha, Digest32] = shaHashFor[Digest32]("Sha-256")

  implicit val sha512: Hash[Sha, Digest64] = shaHashFor[Digest64]("Sha-512")

}
