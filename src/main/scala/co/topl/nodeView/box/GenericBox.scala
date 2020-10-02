package co.topl.nodeView.box

import co.topl.nodeView.box.proposition.Proposition
import co.topl.utils.serialization.BytesSerializable

/**
  * Created by cykoz on 4/13/17.
  */
trait GenericBox[P <: Proposition, T] extends BytesSerializable {
  val value: T
  val proposition: P

  val id: Array[Byte]
}
