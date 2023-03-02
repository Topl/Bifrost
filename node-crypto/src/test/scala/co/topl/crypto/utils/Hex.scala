package co.topl.crypto.utils

import co.topl.models.Bytes
import co.topl.models.utility.HasLength.instances.bytesLength
import co.topl.models.utility.{Length, Sized}

object Hex {

  def encode(bytes: Array[Byte]): String =
    bytes.map("%02X" format _).mkString.toLowerCase

  def encode(bytes: Bytes): String = encode(bytes.toArray)

  def decode(hexString: String): Array[Byte] =
    hexString.replaceAll("[^0-9A-Fa-f]", "").sliding(2, 2).toArray.map(Integer.parseInt(_, 16).toByte)

  def hexStringToStrictBytes[L <: Length](hexString: String)(implicit length: L): Sized.Strict[Bytes, L] =
    Sized.strictUnsafe(Bytes(Hex.decode(hexString)))

  object implicits {

    // will work on any string but the underlying decode implements a regex filter so the method should be safe enough for using in testing
    implicit class Ops(hexString: String) {
      def unsafeStrictBytes[L <: Length](implicit length: L): Sized.Strict[Bytes, L] = hexStringToStrictBytes(hexString)
      def hexStringToBytes: Bytes = Bytes(decode(hexString))
      def hexStringToArrayBytes: Array[Byte] = decode(hexString)
    }
  }
}
