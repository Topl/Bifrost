package co.topl.attestation.keyManagement

import scala.math.BigInt

package object mnemonic {

  private[mnemonic] val byteLen = 8
  private[mnemonic] val indexLen = 11

  /**
   * Converts a mnemonic index into a binary representation.
   * @param i the index to convert
   * @return the binary representation as a `String`
   */
  private[mnemonic] def toBinaryIndex(i: Int): String =
    String.format("%11s", BigInt(i).toString(2)).replace(' ', '0')

  /**
   * Converts a byte to a binary string.
   * @param b the byte to convert
   * @return the binary representation as a `String`
   */
  private[mnemonic] def toBinaryByte(b: Byte): String =
    String.format("%8s", BigInt(b & 0xff).toString(2)).replace(' ', '0')

}
