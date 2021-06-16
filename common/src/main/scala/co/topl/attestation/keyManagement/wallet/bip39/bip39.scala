package co.topl.attestation.keyManagement.wallet

import scala.math.BigInt

package object bip39 {

  val byteLen = 8
  val indexLen = 11

  def toBinaryIndex(i: Int): String = String.format("%11s", BigInt(i).toString(2)).replace(' ', '0')

  def toBinaryByte(b: Byte): String = String.format("%8s", BigInt(b & 0xff).toString(2)).replace(' ', '0')

}
