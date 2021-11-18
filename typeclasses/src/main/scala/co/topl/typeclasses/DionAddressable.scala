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

    implicit val curve25519KnowledgePropositionPubKeyHashDionAddressable
      : DionAddressable[Propositions.Knowledge.Curve25519] = new DionAddressable[Knowledge.Curve25519] {

      def dionAddressOf(t: Knowledge.Curve25519)(implicit networkPrefix: NetworkPrefix): DionAddress =
        DionAddress(
          networkPrefix,
          TypedEvidence(1: Byte, Sized.strictUnsafe(Bytes(blake2b256.hash(t.key.bytes.data.toArray).value)))
        )
    }

    implicit val curve25519ThresholdPropositionDionAddressable
      : DionAddressable[Propositions.Knowledge.Threshold.Curve25519] =
      new DionAddressable[Propositions.Knowledge.Threshold.Curve25519] {

        def dionAddressOf(t: Propositions.Knowledge.Threshold.Curve25519)(implicit
          networkPrefix:     NetworkPrefix
        ): DionAddress =
          DionAddress(
            networkPrefix,
            TypedEvidence(
              2: Byte,
              Sized.strictUnsafe(
                Bytes(
                  blake2b256
                    .hash(
                      Bytes
                        .concat(
                          Bytes(VLQWriter.uLongSerializer(t.threshold)) ::
                          Bytes(VLQWriter.uLongSerializer(t.propositions.size)) ::
                          t.propositions.toList.map(_.bytes.data)
                        )
                        .toArray
                    )
                    .value
                )
              )
            )
          )
      }

    implicit val ed25519KnowledgePropositionPubKeyHashDionAddressable: DionAddressable[Propositions.Knowledge.Ed25519] =
      new DionAddressable[Propositions.Knowledge.Ed25519] {

        def dionAddressOf(
          t:                      Propositions.Knowledge.Ed25519
        )(implicit networkPrefix: NetworkPrefix): DionAddress =
          DionAddress(
            networkPrefix,
            TypedEvidence(
              1: Byte,
              Sized.strictUnsafe(Bytes(blake2b256.hash(t.key.bytes.data.toArray).value))
            )
          )
      }

    implicit val extendedEd25519KnowledgePropositionPubKeyHashDionAddressable
      : DionAddressable[Propositions.Knowledge.ExtendedEd25519] =
      new DionAddressable[Propositions.Knowledge.ExtendedEd25519] {

        def dionAddressOf(
          t:                      Propositions.Knowledge.ExtendedEd25519
        )(implicit networkPrefix: NetworkPrefix): DionAddress =
          DionAddress(
            networkPrefix,
            TypedEvidence(
              5: Byte,
              Sized.strictUnsafe(Bytes(blake2b256.hash((t.key.vk.bytes.data ++ t.key.chainCode.data).toArray).value))
            )
          )
      }

    implicit val ed25519ThresholdPropositionDionAddressable: DionAddressable[Propositions.Knowledge.Threshold.Ed25519] =
      new DionAddressable[Propositions.Knowledge.Threshold.Ed25519] {

        def dionAddressOf(t: Propositions.Knowledge.Threshold.Ed25519)(implicit
          networkPrefix:     NetworkPrefix
        ): DionAddress =
          DionAddress(
            networkPrefix,
            TypedEvidence(
              2: Byte,
              Sized.strictUnsafe(
                Bytes(
                  blake2b256
                    .hash(
                      Bytes
                        .concat(
                          Bytes(VLQWriter.uLongSerializer(t.threshold)) ::
                          Bytes(VLQWriter.uLongSerializer(t.propositions.size)) ::
                          t.propositions.toList.map(_.bytes.data)
                        )
                        .toArray
                    )
                    .value
                )
              )
            )
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
