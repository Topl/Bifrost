package co.topl.crypto.hash

import scorex.utils.ByteArray
import Hash.Digest32

import java.security.MessageDigest
import scala.util.Try

object Sha256 {
  implicit val digest32: HashFunction[Digest32] = new HashFunction[Digest32] {

    override val digestSize = 32

    override def apply(input: Message): Digest32 =
      Digest32 @@ MessageDigest.getInstance("SHA-256").digest(input)

    override def apply(prefix: Byte, inputs: Array[Byte]*): Digest32 =
      apply(prefix +: ByteArray.concat(inputs))

    override def byteArrayToDigest(bytes: Array[Byte]): Try[Digest32] = Try {
      require(bytes.lengthCompare(digestSize) == 0, "Incorrect digest size")
      Digest32 @@ bytes
    }

  }
}
