package bifrost.scorexMod

import scorex.core.serialization.BytesSerializable
import scorex.core.transaction.box.proposition.Proposition

/**
  * Created by cykoz on 4/13/17.
  */
trait GenericBox[P <: Proposition, T] extends BytesSerializable {
  val value: T
  val proposition: P

  val id: Array[Byte]
}
