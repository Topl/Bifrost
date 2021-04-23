package co.topl.crypto.hash

import cats.implicits._
import cats.data.NonEmptyChain

import java.security.MessageDigest

case class Sha256()

object Sha256 {

  /** Sha256 hashing function implementation. */
  implicit val hash: Hash[Sha256] = new Hash[Sha256] {

    override val digestSize = 32

    override def hash(prefix: Option[Byte], messages: NonEmptyChain[Array[Byte]]): Digest = {
      val hashBytes = MessageDigest
        .getInstance("SHA-256")
        .digest(
          messages.foldLeft(prefix.map(Array[Byte](_))
            .getOrElse(Array[Byte]()))(_ ++ _)
        )

      Digest(hashBytes)
    }

  }

}
