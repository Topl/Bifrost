package co.topl.crypto.utils

import java.math.BigInteger

object Hex {

  def encode(bytes: Array[Byte]): String =
    bytes.map("%02X" format _).mkString

  def decode(hexString: String): Array[Byte] =
    hexString.replaceAll("[^0-9A-Fa-f]", "").sliding(2, 2).toArray.map(Integer.parseInt(_, 16).toByte)
}
