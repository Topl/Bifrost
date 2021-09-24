package co.topl.crypto.typeclasses

import co.topl.models._
import simulacrum.{op, typeclass}

@typeclass trait KesCertifies[T] {
  @op("certify") def certifyWith(t: T, unsignedBlock: BlockHeaderV2.Unsigned): OperationalCertificate
}

object KesCertifies {

  trait Instances {}

  object instances extends Instances
}
