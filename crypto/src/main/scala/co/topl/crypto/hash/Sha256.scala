package co.topl.crypto.hash

import java.security.MessageDigest
import scala.util.Try

case class Sha256()

object Sha256 {

  /** Sha256 hashing function implementation. */
  implicit val hash: Hash[Sha256] = new Hash[Sha256] {

    override val digestSize = 32

    override def hash(input: Array[Byte]): Digest =
      Digest @@ MessageDigest.getInstance("SHA-256").digest(input)

    override def hashWithPrefix(prefix: Byte, inputs: Array[Byte]*): Digest =
      hash(prefix +: inputs.foldLeft(Array[Byte]())(_ ++ _))

    override def byteArrayToDigest(bytes: Array[Byte]): Try[Digest] = Try {
      require(bytes.lengthCompare(digestSize) == 0, "Incorrect digest size")
      Digest @@ bytes
    }

  }

}
