package co.topl.modifier.transaction.builder

import co.topl.attestation.Address
import co.topl.modifier.box.{ArbitBox, AssetBox, PolyBox}

final case class BoxSet(
  arbits: Set[(Address, ArbitBox)],
  polys:  Set[(Address, PolyBox)],
  assets: Set[(Address, AssetBox)]
)

object BoxSet {
  def empty: BoxSet = BoxSet(Set.empty, Set.empty, Set.empty)
}
