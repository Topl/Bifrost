package co.topl.nodeView.state.box

import co.topl.crypto.Proof
import co.topl.nodeView.state.box.proposition.Proposition
import scorex.util.encode.Base58

trait BoxUnlocker[+P <: Proposition] {
  val closedBoxId: Array[Byte]
  val boxKey: Proof[P]

  override def toString: String = s"BoxUnlocker(id: ${Base58.encode(closedBoxId)}, boxKey: $boxKey)"
}
