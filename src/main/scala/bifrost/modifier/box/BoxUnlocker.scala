package bifrost.modifier.box

import bifrost.crypto.Proof
import bifrost.modifier.box.proposition.Proposition
import scorex.util.encode.Base58

trait BoxUnlocker[+P <: Proposition] {
  val closedBoxId: Array[Byte]
  val boxKey: Proof[P]

  override def toString: String = s"BoxUnlocker(id: ${Base58.encode(closedBoxId)}, boxKey: $boxKey)"
}
