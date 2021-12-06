package co.topl.modifier.transaction

import co.topl.attestation.Address
import co.topl.modifier.box.{ArbitBox, AssetBox, PolyBox}

package object builder {

  /**
   * Helper type representing a set of available boxes for use in a transaction.
   * @param arbits the available arbit boxes
   * @param polys the available poly boxes
   * @param assets the available asset boxes
   */
  private[builder] case class TokenBoxes(
    arbits: List[(Address, ArbitBox)],
    polys:  List[(Address, PolyBox)],
    assets: List[(Address, AssetBox)]
  )
}
