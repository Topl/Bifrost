package co.topl.typeclasses

import co.topl.codecs.bytes.VLQWriter
import co.topl.crypto.hash.blake2b256
import co.topl.models._

@simulacrum.typeclass
trait DionAddressable[T] {

  @simulacrum.op("dionAddress")
  def dionAddressOf(t: T): DionAddress
}

object DionAddressable {

  trait Instances {

    implicit val curve25519KnowledgePropositionPubKeyHashDionAddressable
      : DionAddressable[Propositions.Knowledge.Curve25519] =
      t => TypedBytes(1: Byte, Bytes(blake2b256.hash(t.key.bytes.data.toArray).value))

    implicit val curve25519ThresholdPropositionDionAddressable
      : DionAddressable[Propositions.Knowledge.Threshold.Curve25519] =
      t =>
        TypedBytes(
          2: Byte,
          Bytes.concat(
            Bytes(VLQWriter.uLongSerializer(t.threshold)) ::
            Bytes(VLQWriter.uLongSerializer(t.propositions.size)) ::
            t.propositions.toList.map(_.bytes.data)
          )
        )

    implicit val ed25519KnowledgePropositionPubKeyHashDionAddressable: DionAddressable[Propositions.Knowledge.Ed25519] =
      t => TypedBytes(3: Byte, Bytes(blake2b256.hash(t.key.bytes.data.toArray).value))

    implicit val extendeddEd25519KnowledgePropositionPubKeyHashDionAddressable
      : DionAddressable[Propositions.Knowledge.ExtendedEd25519] =
      t => TypedBytes(3: Byte, Bytes(blake2b256.hash((t.key.vk.bytes.data ++ t.key.chainCode.data).toArray).value))

    implicit val ed25519ThresholdPropositionDionAddressable: DionAddressable[Propositions.Knowledge.Threshold.Ed25519] =
      t =>
        TypedBytes(
          4: Byte,
          Bytes.concat(
            Bytes(VLQWriter.uLongSerializer(t.threshold)) ::
            Bytes(VLQWriter.uLongSerializer(t.propositions.size)) ::
            t.propositions.toList.map(_.bytes.data)
          )
        )

    implicit def proposesAddressable[T, Prop <: Proposition](implicit
      proposer:        Proposer[T, Prop],
      dionAddressable: DionAddressable[Prop]
    ): DionAddressable[T] =
      t => dionAddressable.dionAddressOf(proposer.propositionOf(t))
  }
}
