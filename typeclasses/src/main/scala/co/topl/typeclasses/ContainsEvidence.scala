package co.topl.typeclasses

import co.topl.codecs.bytes.VLQWriter
import co.topl.codecs.bytes.implicits._
import co.topl.crypto.hash.blake2b256
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.{Ratio, Sized}
import co.topl.models._
import com.google.common.primitives.{Ints, Longs}
import simulacrum.{op, typeclass}

import java.nio.charset.StandardCharsets

@typeclass trait ContainsEvidence[T] {
  @op("typedEvidence") def typedEvidenceOf(t: T): TypedEvidence
}

object ContainsEvidence {

  trait Instances {

    implicit val ratioContainsEvidence: ContainsEvidence[Ratio] =
      ratio => TypedEvidence(10: Byte, Sized.strictUnsafe(Bytes(blake2b256.hash(ratio.bytes.toArray).value)))

    implicit val curve25519VKContainsEvidence: ContainsEvidence[VerificationKeys.Curve25519] =
      t => TypedEvidence(1: Byte, Sized.strictUnsafe(Bytes(blake2b256.hash(t.bytes.data.toArray).value)))

    implicit val ed25519VKContainsEvidence: ContainsEvidence[VerificationKeys.Ed25519] =
      t => TypedEvidence(2: Byte, Sized.strictUnsafe(Bytes(blake2b256.hash(t.bytes.data.toArray).value)))

    implicit val extended25519VKContainsEvidence: ContainsEvidence[VerificationKeys.ExtendedEd25519] =
      t =>
        TypedEvidence(3: Byte, Sized.strictUnsafe(Bytes(blake2b256.hash((t.bytes ++ t.chainCode.data).toArray).value)))

    implicit val vkContainsEvidence: ContainsEvidence[VerificationKey] = {
      case t: VerificationKeys.Curve25519      => curve25519VKContainsEvidence.typedEvidenceOf(t)
      case t: VerificationKeys.Ed25519         => ed25519VKContainsEvidence.typedEvidenceOf(t)
      case t: VerificationKeys.ExtendedEd25519 => extended25519VKContainsEvidence.typedEvidenceOf(t)
      case t                                   => throw new MatchError(t)
    }

    implicit val permanentlyLockedContainsEvidence: ContainsEvidence[Propositions.PermanentlyLocked.type] =
      t =>
        TypedEvidence(
          9: Byte,
          Sized.strictUnsafe(Bytes(blake2b256.hash("LOCKED".getBytes(StandardCharsets.UTF_8)).value))
        )

    implicit val curve25519KnowledgePropositionContainsEvidence: ContainsEvidence[Propositions.Knowledge.Curve25519] =
      t => TypedEvidence(1: Byte, Sized.strictUnsafe(Bytes(blake2b256.hash(t.key.bytes.data.toArray).value)))

    implicit val ed25519KnowledgePropositionContainsEvidence: ContainsEvidence[Propositions.Knowledge.Ed25519] =
      t => TypedEvidence(3: Byte, Sized.strictUnsafe(Bytes(blake2b256.hash(t.key.bytes.data.toArray).value)))

    implicit val extendedEd25519KnowledgePropositionContainsEvidence
      : ContainsEvidence[Propositions.Knowledge.ExtendedEd25519] =
      t =>
        TypedEvidence(
          5: Byte,
          Sized.strictUnsafe(Bytes(blake2b256.hash((t.key.vk.bytes.data ++ t.key.chainCode.data).toArray).value))
        )

    implicit def thresholdContainsEvidence(implicit
      ev: ContainsEvidence[Proposition]
    ): ContainsEvidence[Propositions.Compositional.Threshold] =
      t =>
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
                      t.propositions.toList.map(p => ev.typedEvidenceOf(p).allBytes)
                    )
                    .toArray
                )
                .value
            )
          )
        )

    implicit def andContainsEvidence(implicit
      ev: ContainsEvidence[Proposition]
    ): ContainsEvidence[Propositions.Compositional.And] =
      t =>
        TypedEvidence(
          6: Byte,
          Sized.strictUnsafe(
            Bytes(
              blake2b256
                .hash((ev.typedEvidenceOf(t.a).allBytes ++ ev.typedEvidenceOf(t.b).allBytes).toArray)
                .value
            )
          )
        )

    implicit def orContainsEvidence(implicit
      ev: ContainsEvidence[Proposition]
    ): ContainsEvidence[Propositions.Compositional.Or] =
      t =>
        TypedEvidence(
          7: Byte,
          Sized.strictUnsafe(
            Bytes(
              blake2b256
                .hash((ev.typedEvidenceOf(t.a).allBytes ++ ev.typedEvidenceOf(t.b).allBytes).toArray)
                .value
            )
          )
        )

    implicit val heightLockContainsEvidence: ContainsEvidence[Propositions.Contextual.HeightLock] =
      t =>
        TypedEvidence(
          8: Byte,
          Sized.strictUnsafe(
            Bytes(
              blake2b256
                .hash(Longs.toByteArray(t.height))
                .value
            )
          )
        )

//    implicit val requiredOutputContainsEvidence: ContainsEvidence[Propositions.Contextual.RequiredDionOutput] =
//      t =>
//        TypedEvidence(
//          9: Byte,
//          Sized.strictUnsafe(
//            Bytes(
//              blake2b256
//                .hash(Ints.toByteArray(t.index) ++ t.address.allBytes.toArray)
//                .value
//            )
//          )
//        )

    implicit val requiredInputBoxStateContainsEvidence: ContainsEvidence[Propositions.Contextual.RequiredBoxState] =
      t => {
        val locationPrefix = t.location match {
          case BoxLocations.Input  => 0: Byte
          case BoxLocations.Output => 1: Byte
        }

        TypedEvidence(
          15: Byte,
          Sized.strictUnsafe(
            Bytes(
              blake2b256
                .hash(
                  locationPrefix +: t.boxes
                    .map { case (index, box) => Ints.toByteArray(index) ++ Longs.toByteArray(box.nonce) }
                    .foldLeft(Array.empty[Byte])((acc, a) => acc ++ a)
                )
                .value
            )
          )
        )
      }

    implicit val enumeratedOutputContainsEvidence: ContainsEvidence[Propositions.Example.EnumeratedInput] =
      t =>
        TypedEvidence(
          10: Byte,
          Sized.strictUnsafe(
            Bytes(
              blake2b256
                .hash(t.values.map(Ints.toByteArray).foldLeft(Array.empty[Byte])((acc, a) => acc ++ a))
                .value
            )
          )
        )

    implicit val commitRevealContainsEvidence: ContainsEvidence[Propositions.Knowledge.HashLock] =
      t =>
        TypedEvidence(
          11: Byte,
          Sized.strictUnsafe(
            Bytes(
              blake2b256
                .hash(t.digest.data.toArray)
                .value
            )
          )
        )

    implicit val jsScriptPropositionContainsEvidence: ContainsEvidence[Propositions.Script.JS] =
      t =>
        TypedEvidence(
          9: Byte,
          Sized.strictUnsafe(
            Bytes(
              blake2b256
                .hash(t.script.value.getBytes(StandardCharsets.UTF_8))
                .value
            )
          )
        )

    implicit lazy val propositionContainsEvidence: ContainsEvidence[Proposition] = {
      case Propositions.PermanentlyLocked =>
        permanentlyLockedContainsEvidence.typedEvidenceOf(Propositions.PermanentlyLocked)

      case t: Propositions.Knowledge.Curve25519 => curve25519KnowledgePropositionContainsEvidence.typedEvidenceOf(t)
      case t: Propositions.Knowledge.Ed25519    => ed25519KnowledgePropositionContainsEvidence.typedEvidenceOf(t)
      case t: Propositions.Knowledge.ExtendedEd25519 =>
        extendedEd25519KnowledgePropositionContainsEvidence.typedEvidenceOf(t)

      case t: Propositions.Compositional.And => andContainsEvidence(propositionContainsEvidence).typedEvidenceOf(t)
      case t: Propositions.Compositional.Or  => orContainsEvidence(propositionContainsEvidence).typedEvidenceOf(t)
      case t: Propositions.Compositional.Threshold =>
        thresholdContainsEvidence(propositionContainsEvidence).typedEvidenceOf(t)

      case t: Propositions.Contextual.HeightLock => heightLockContainsEvidence.typedEvidenceOf(t)

      case t: Propositions.Script.JS => jsScriptPropositionContainsEvidence.typedEvidenceOf(t)

      case t: Propositions.Knowledge.HashLock          => commitRevealContainsEvidence.typedEvidenceOf(t)
      case t: Propositions.Example.EnumeratedInput     => enumeratedOutputContainsEvidence.typedEvidenceOf(t)
      case t: Propositions.Contextual.RequiredBoxState => requiredInputBoxStateContainsEvidence.typedEvidenceOf(t)
    }
  }

  object Instances extends Instances
}
