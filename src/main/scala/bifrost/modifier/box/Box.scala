package bifrost.modifier.box

import bifrost.serialization.BytesSerializable
import bifrost.modifier.box.proposition.Proposition

/**
  * Box is a state element locked by some proposition.
  */
trait Box[P <: Proposition] extends BytesSerializable {
  val value: Box.Amount
  val proposition: P

  val id: Array[Byte]
}

object Box {
  type Amount = Long
}