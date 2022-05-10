package co.topl.typeclasses

import co.topl.models._

@simulacrum.typeclass
trait DionAddressable[T] {

  @simulacrum.op("dionAddress")
  def dionAddressOf(t: T): DionAddress
}

object DionAddressable {

  trait Instances {

    implicit def containsEvidenceDionAddressable[T: ContainsEvidence](implicit
      networkPrefix: NetworkPrefix
    ): DionAddressable[T] =
      (t: T) =>
        DionAddress(
          networkPrefix,
          ContainsEvidence[T].typedEvidenceOf(t)
        )
  }
}
