package co.topl.nodeView.state.box

import co.topl.crypto.Proof
import co.topl.nodeView.state.box.proposition.Proposition
import scorex.crypto.encode.Base58

trait BoxUnlocker[+P <: Proposition] {
  val closedBoxId: BoxId
  val boxKey: Proof[P]

  override def toString: String = s"BoxUnlocker(id: $closedBoxId, boxKey: $boxKey)"
}
