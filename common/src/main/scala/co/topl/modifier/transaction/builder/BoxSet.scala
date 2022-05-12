package co.topl.modifier.transaction.builder

import co.topl.attestation.Address
import co.topl.models.BoxReference
import co.topl.modifier.box.{ArbitBox, AssetBox, PolyBox}
import co.topl.utils.Int128

final case class BoxSet(
  arbits: List[(Address, ArbitBox)],
  polys:  List[(Address, PolyBox)],
  assets: List[(Address, AssetBox)]
)

object BoxSet {
  def empty: BoxSet = BoxSet(Nil, Nil, Nil)
}
