package co.topl

import co.topl.crypto.hash.digest

package object crypto {

  object implicits
      extends digest.Instances
      with digest.Digest.ToDigestOps
      with digest.Extensions
      with hash.Instances
      with hash.ToHashResultOps
      with signatures.ToCreateKeyPairResultOps
      with signatures.PrivateKey.Instances

  object Hex {

    def encode(bytes: Array[Byte]): String =
      bytes.map("%02X" format _).mkString

    def decode(hexString: String): Array[Byte] =
      hexString.replaceAll("[^0-9A-Fa-f]", "").sliding(2, 2).toArray.map(Integer.parseInt(_, 16).toByte)
  }
}
