package co.topl.crypto.hash

import co.topl.crypto.BytesOf
import co.topl.crypto.Implicits._

import java.security.MessageDigest

object Sha {

  case class Sha256()

  case class Sha512()

  object Sha256 {

    implicit val digest32: Hash[Sha256, Digest32] = new Hash[Sha256, Digest32] {
      override def hash[V: BytesOf](prefix: Option[Byte], messages: V*): Digest32 =
        Digest32(Sha.hash(prefix, messages, "Sha-256"))
    }
  }

  object Sha512 {

    implicit val digest64: Hash[Sha512, Digest64] = new Hash[Sha512, Digest64] {
      override def hash[V: BytesOf](prefix: Option[Byte], messages: V*): Digest64 =
        Digest64(Sha.hash(prefix, messages, "Sha-512"))
    }
  }

  /** Java Security Standard Names --
   * https://docs.oracle.com/en/java/javase/11/docs/specs/security/standard-names.html */
  private def hash[V: BytesOf](prefix: Option[Byte], messages: Seq[V], algorithm: String): Array[Byte] = {
    MessageDigest
      .getInstance(algorithm)
      .digest(
        messages.foldLeft(prefix.map(Array[Byte](_))
          .getOrElse(Array[Byte]()))(BytesOf[Array[Byte]].concat)
      )
  }
}
