package co.topl.nodeView.box

import co.topl.crypto.Proof
import co.topl.nodeView.box.proposition.Proposition
import scorex.crypto.encode.Base58

trait BoxUnlocker[+P <: Proposition] {
  val closedBoxId: Array[Byte]
  val boxKey: Proof[P]

  override def toString: String = s"BoxUnlocker(id: ${Base58.encode(closedBoxId)}, boxKey: $boxKey)"
}
