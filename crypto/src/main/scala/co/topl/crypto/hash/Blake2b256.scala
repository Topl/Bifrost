package co.topl.crypto.hash

import org.bouncycastle.crypto.digests.Blake2bDigest
import Hash.Digest32

import scala.util.Try

object Blake2b256 {
  implicit val digest32: HashFunction[Digest32] = new HashFunction[Digest32] {

    override val digestSize = 32
    lazy val digestFn: Blake2bDigest = new Blake2bDigest(digestSize)

    override def apply(input: Array[Byte]): Digest32 = {
      Digest32 @@ synchronized {
        digestFn.update(input, 0, input.length)
        val res = new Array[Byte](digestSize)
        digestFn.doFinal(res, 0)
        res
      }
    }

    override def apply(prefix: Byte, inputs: Array[Byte]*): Digest32 = {
      Digest32 @@ synchronized {
        digestFn.update(prefix)
        inputs.foreach(i => digestFn.update(i, 0, i.length))
        val res = new Array[Byte](digestSize)
        digestFn.doFinal(res, 0)
        res
      }
    }

    override def byteArrayToDigest(bytes: Array[Byte]): Try[Digest32] = Try {
      require(bytes.lengthCompare(digestSize) == 0, "Incorrect digest size")
      Digest32 @@ bytes
    }

  }
}
