package co.topl.nodeView.state.box

import co.topl.crypto.{ Proof, Proposition }

sealed abstract class Unlocker[+P <: Proposition]

class BoxUnlocker[+P <: Proposition](val closedBoxId: BoxId, val boxKey: Proof[P]) extends Unlocker[P] {

  override def toString: String = s"BoxUnlocker(id: $closedBoxId, boxKey: $boxKey)"
}