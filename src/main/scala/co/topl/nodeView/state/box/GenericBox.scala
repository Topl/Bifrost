package co.topl.nodeView.state.box

import co.topl.nodeView.state.box.proposition.Proposition
import co.topl.utils.serialization.{ BytesSerializable, JsonEncodable, JsonSerializable }

/**
  * Created by cykoz on 4/13/17.
  */
trait GenericBox[P <: Proposition, T] extends BytesSerializable with JsonEncodable {
  val value: T
  val proposition: P

  val id: BoxId
}
