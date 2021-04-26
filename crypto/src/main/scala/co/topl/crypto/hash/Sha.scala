package co.topl.crypto.hash

import cats.implicits._
import cats.data.NonEmptyChain

import java.security.MessageDigest

object Sha {

  case class Sha256()

  case class Sha512()

  object Sha256 {

    implicit val digest32: Hash[Sha256, Digest32] = new Hash[Sha256, Digest32] {

      override val digestSize = 32

      override def hash(prefix: Option[Byte], messages: NonEmptyChain[Array[Byte]]): Digest32 =
        Digest32(Sha.hash(prefix, messages, "Sha-256"))

    }

  }

  object Sha512 {

    implicit val digest32: Hash[Sha256, Digest32] = new Hash[Sha256, Digest32] {

      override val digestSize = 32

      override def hash(prefix: Option[Byte], messages: NonEmptyChain[Array[Byte]]): Digest32 =
        Digest32(Sha.hash(prefix, messages, "Sha-512"))

    }
  }

  private def hash(prefix: Option[Byte], messages: NonEmptyChain[Array[Byte]], algorithm: String): Array[Byte] =
    MessageDigest
      .getInstance(algorithm)
      .digest(
        messages.foldLeft(prefix.map(Array[Byte](_))
          .getOrElse(Array[Byte]()))(_ ++ _)
      )
}
