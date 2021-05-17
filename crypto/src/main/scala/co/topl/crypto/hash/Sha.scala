package co.topl.crypto.hash

import co.topl.crypto.hash.digest.{Digest, Digest32, Digest64}
import co.topl.crypto.hash.implicits._

import java.security.MessageDigest

abstract class ShaHash[D: Digest](val algorithmName: String) extends Hash[Sha, D] {

  override def hash(prefix: Option[Byte], messages: Message*): HashResult[D] =
    Digest[D]
      .from(
        MessageDigest
          .getInstance(algorithmName)
          .digest(
            messages.foldLeft(
              prefix
                .map(Array[Byte](_))
                .getOrElse(Array[Byte]())
            )(_ ++ _)
          )
      )
      .leftMap(InvalidDigestFailure)
      .toEither
}

case object Sha256 extends ShaHash[Digest32]("Sha-256")
case object Sha512 extends ShaHash[Digest64]("Sha-512")
