package bifrost.nodeView.box

import bifrost.nodeView.box.proposition.Proposition
import bifrost.utils.serialization.BytesSerializable

/**
  * Created by cykoz on 4/13/17.
  */
trait GenericBox[P <: Proposition, T] extends BytesSerializable {
  val value: T
  val proposition: P

  val id: Array[Byte]
}
