package co.topl.typeclasses

import co.topl.models._

@simulacrum.typeclass
trait DionAddressable[T] {

  @simulacrum.op("dionAddress")
  def dionAddressOf(t: T)(implicit networkPrefix: NetworkPrefix): DionAddress
}

object DionAddressable {

  trait Instances {

    implicit def containsEvidenceDionAddressable[T: ContainsEvidence](implicit
      networkPrefix: NetworkPrefix
    ): DionAddressable[T] =
      new DionAddressable[T] {

        def dionAddressOf(t: T)(implicit networkPrefix: NetworkPrefix): DionAddress =
          DionAddress(
            networkPrefix,
            ContainsEvidence[T].typedEvidenceOf(t)
          )
      }
  }
}
