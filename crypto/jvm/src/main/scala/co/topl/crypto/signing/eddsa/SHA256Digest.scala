package co.topl.crypto.signing.eddsa

import java.security.MessageDigest

private[signing] class SHA512Digest {

  val digest: MessageDigest = MessageDigest.getInstance("SHA-512")

  def Sha512(bytes: Array[Byte]): Array[Byte] = {
    digest.reset()
    digest.update(bytes)
    digest.digest()
  }

  def getAlgorithmName: String = "SHA-512"

  def getDigestSize: Int = 64

  def update(in: Byte): Unit = digest.update(in)

  def update(in: Array[Byte], inOff: Int, len: Int): Unit = digest.update(in, inOff, len)

  def doFinal(out: Array[Byte], outOff: Int): Int =
    digest.digest(out, outOff, out.length)

  def reset(): Unit = digest.reset()
}

private[signing] class SHA256Digest {

  val digest: MessageDigest = MessageDigest.getInstance("SHA-256")

  def Sha256(bytes: Array[Byte]): Array[Byte] = {
    digest.reset()
    digest.update(bytes)
    digest.digest()
  }

  def getAlgorithmName: String = "SHA-256"

  def getDigestSize: Int = 32

  def update(in: Byte): Unit = digest.update(in)

  def update(in: Array[Byte], inOff: Int, len: Int): Unit = digest.update(in, inOff, len)

  def doFinal(out: Array[Byte], outOff: Int): Int =
    digest.digest(out, outOff, out.length)

  def reset(): Unit = digest.reset()
}
