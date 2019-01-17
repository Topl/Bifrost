package bifrost.transaction.box

import bifrost.transaction.box.proposition.Proposition
import bifrost.transaction.proof.Proof
import scorex.crypto.encode.Base58

trait BoxUnlocker[+P <: Proposition] {
  val closedBoxId: Array[Byte]
  val boxKey: Proof[P]

  override def toString: String = s"BoxUnlocker(id: ${Base58.encode(closedBoxId)}, boxKey: $boxKey)"
}
