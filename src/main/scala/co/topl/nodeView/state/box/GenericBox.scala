package co.topl.nodeView.state.box

import co.topl.attestation.Proposition
import co.topl.utils.serialization.{BytesSerializable, JsonEncodable}

/**
  * Created by cykoz on 4/13/17.
  */
trait GenericBox[P <: Proposition, T] extends BytesSerializable with JsonEncodable {
  val value: T
  val proposition: P

  val id: BoxId
}
