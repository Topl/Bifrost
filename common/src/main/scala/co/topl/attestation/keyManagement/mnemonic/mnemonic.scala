package co.topl.attestation.keyManagement

import scala.math.BigInt

package object mnemonic {

  private[mnemonic] val byteLen = 8
  private[mnemonic] val indexLen = 11

  /**
   * Converts an integer into a binary representation with 11 bits.
   * @param i the index to convert
   * @return the 11-bit binary representation as a `String`
   */
  private[mnemonic] def toBinaryStringWith11Bits(i: Int): String =
    String.format("%11s", BigInt(i).toString(2)).replace(' ', '0')

  /**
   * Converts a byte to a binary string.
   * @param b the byte to convert
   * @return the binary representation as a `String`
   */
  private[mnemonic] def toBinaryString(b: Byte): String =
    String.format("%8s", BigInt(b & 0xff).toString(2)).replace(' ', '0')

}
