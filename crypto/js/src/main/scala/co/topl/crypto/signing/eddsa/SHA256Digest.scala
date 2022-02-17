package co.topl.crypto.signing.eddsa

private[signing] class SHA512Digest {

  def Sha512(bytes: Array[Byte]): Array[Byte] =
    ???

  def getAlgorithmName: String = "SHA-512"

  def getDigestSize: Int = 64

  def update(in: Byte): Unit = ???

  def update(in: Array[Byte], inOff: Int, len: Int): Unit = ???

  def doFinal(out: Array[Byte], outOff: Int): Int =
    ???

  def reset(): Unit = ???
}

private[signing] class SHA256Digest {

  def Sha256(bytes: Array[Byte]): Array[Byte] =
    ???

  def getAlgorithmName: String = "SHA-256"

  def getDigestSize: Int = 32

  def update(in: Byte): Unit = ???

  def update(in: Array[Byte], inOff: Int, len: Int): Unit = ???

  def doFinal(out: Array[Byte], outOff: Int): Int =
    ???

  def reset(): Unit = ???
}
