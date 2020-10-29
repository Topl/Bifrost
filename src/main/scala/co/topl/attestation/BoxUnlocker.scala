package co.topl.attestation

import co.topl.attestation.proof.Proof
import co.topl.nodeView.state.box.BoxId

sealed abstract class Unlocker[+P <: Proposition]

class BoxUnlocker[+P <: Proposition](val closedBoxId: BoxId, val boxKey: Proof[P]) extends Unlocker[P] {

  override def toString: String = s"BoxUnlocker(id: $closedBoxId, boxKey: $boxKey)"
}