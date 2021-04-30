package co.topl.crypto.utils

object Hex {

  def encode(bytes: Array[Byte]): String =
    bytes.map("%02X" format _).mkString

}
