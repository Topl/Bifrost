package co.topl.typeclasses

import co.topl.codecs.bytes.VLQWriter
import co.topl.crypto.hash.blake2b256
import co.topl.models.Propositions.Knowledge
import co.topl.models._
import co.topl.models.utility.HasLength.instances.bytesLength
import co.topl.models.utility.Sized

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

    implicit def proposesAddressable[T, Prop <: Proposition](implicit
      proposer:        Proposer[T, Prop],
      dionAddressable: DionAddressable[Prop]
    ): DionAddressable[T] =
      new DionAddressable[T] {

        def dionAddressOf(t: T)(implicit networkPrefix: NetworkPrefix): DionAddress =
          dionAddressable.dionAddressOf(proposer.propositionOf(t))
      }
  }
}
