package co.topl.crypto.generation

/**
 * A mnemonic represents a set of random entropy that can be used to derive a private key or other type of value.
 * This implementation follows a combination of BIP-0039 and SLIP-0023.
 * https://github.com/bitcoin/bips/blob/master/bip-0039.mediawiki
 * https://github.com/satoshilabs/slips/blob/master/slip-0023.md
 */
package object mnemonic {

  /*
   * ENT = entropy
   * CS (checksum) = ENT / 32
   * MS (mnemonic size) = (ENT + CS) / 11
   *
   * |  ENT  | CS | ENT+CS |  MS  |
   * +-------+----+--------+------+
   * |  128  |  4 |   132  |  12  |
   * |  160  |  5 |   165  |  15  |
   * |  192  |  6 |   198  |  18  |
   * |  224  |  7 |   231  |  21  |
   * |  256  |  8 |   264  |  24  |
   *
   */

  private[mnemonic] val byteLen = 8
  private[mnemonic] val indexLen = 11

  /**
   * Converts an integer into a binary representation with 11 bits.
   * @param i the index to convert
   * @return the 11-bit binary representation as a `String`
   */
  private[mnemonic] def intTo11BitString(i: Int): String =
    String.format("%11s", BigInt(i).toString(2)).replace(' ', '0')

  /**
   * Converts a byte to a binary string.
   * @param b the byte to convert
   * @return the binary representation as a `String`
   */
  private[mnemonic] def byteTo8BitString(b: Byte): String =
    String.format("%8s", BigInt(b & 0xff).toString(2)).replace(' ', '0')

  /**
   * Mnemonic size is an enum with additional parameters for calculating checksum and entropy lengths.
   *
   * @param wordLength the size of the mnemonic
   */
  sealed abstract class MnemonicSize(val wordLength: Int) {
    val checksumLength: Int = wordLength / 3
    val entropyLength: Int = 32 * checksumLength
  }

  object MnemonicSizes {
    case object `12` extends MnemonicSize(12)
    case object `15` extends MnemonicSize(15)
    case object `18` extends MnemonicSize(18)
    case object `21` extends MnemonicSize(21)
    case object `24` extends MnemonicSize(24)
  }
}
