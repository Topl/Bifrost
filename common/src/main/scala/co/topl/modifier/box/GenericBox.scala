package co.topl.modifier.box

import co.topl.attestation.Evidence
import co.topl.utils.serialization.BytesSerializable

/**
 * Created by cykoz on 4/13/17.
 */
abstract class GenericBox[+T] extends BytesSerializable {
  val evidence: Evidence // a commitment to the proposition locking this box
  val value: T // a box-type dependent quantity
  val id: BoxId // a one-time only, unique reference id (computed from the input transaction data)

  override def equals(obj: Any): Boolean = obj match {
    case bx: GenericBox[T @unchecked] => (bx.id == id) && bx.value == value && bx.evidence == evidence
    case _                 => false
  }
}
