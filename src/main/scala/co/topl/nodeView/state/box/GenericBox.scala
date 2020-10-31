package co.topl.nodeView.state.box

import co.topl.attestation.{ Evidence, EvidenceProducer }
import co.topl.utils.serialization.{ BytesSerializable, JsonEncodable }

/**
  * Created by cykoz on 4/13/17.
  */
abstract class GenericBox[T] extends BytesSerializable with JsonEncodable {
  val evidence: Evidence // a commitment to the proposition locking this box
  val value: T           // a box-type dependent quantity
  val id: BoxId          // a one-time only, unique reference id (computed from the input transaction data)

  override def equals (obj: Any): Boolean = obj match {
    case bx: GenericBox[T] => (bx.id == id) && bx.value == value && bx.evidence == evidence
    case _                 => false
  }
}
