package co.topl.modifier

import co.topl.attestation.Address
import co.topl.modifier.box.{AssetBox, Box, PolyBox, TokenValueHolder}
import co.topl.utils.Int128

package object transaction {

  def boxFunds(fromBoxes: IndexedSeq[(Address, Box[TokenValueHolder])]): Int128 =
    fromBoxes.map(_._2.value.quantity).sum
}
