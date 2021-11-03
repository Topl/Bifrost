package co.topl.typeclasses

import co.topl.crypto.hash.blake2b256
import co.topl.models._

@simulacrum.typeclass
trait DionAddressable[T] {

  @simulacrum.op("dionAddress")
  def dionAddressOf(t: T): DionAddress
}

object DionAddressable {

  trait Instances {

    implicit val curve25519KnowledgePropositionDionAddressable: DionAddressable[Propositions.Knowledge.Curve25519] =
      t => TypedBytes(1: Byte, Bytes(blake2b256.hash(t.key.bytes.data.toArray).value))

    implicit val curve25519ThresholdPropositionDionAddressable
      : DionAddressable[Propositions.Knowledge.Threshold.Curve25519] =
      t => TypedBytes(2: Byte, ???) // TODO

    implicit val ed25519KnowledgePropositionDionAddressable: DionAddressable[Propositions.Knowledge.Ed25519] =
      t => TypedBytes(3: Byte, Bytes(blake2b256.hash(t.key.bytes.data.toArray).value))

    implicit val ed25519ThresholdPropositionDionAddressable: DionAddressable[Propositions.Knowledge.Threshold.Ed25519] =
      t => TypedBytes(4: Byte, ???) // TODO
  }
}
