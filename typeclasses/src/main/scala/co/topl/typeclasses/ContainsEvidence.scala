package co.topl.typeclasses

import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.ImmutableCodec
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.crypto.hash.Blake2b256
import co.topl.models._
import co.topl.models.utility.Ratio
import simulacrum.{op, typeclass}

@typeclass trait ContainsEvidence[T] {
  @op("typedEvidence") def typedEvidenceOf(t: T): TypedEvidence
}

object ContainsEvidence {

  def fromImmutableCodec[T: ImmutableCodec](prefix: Byte): ContainsEvidence[T] =
    (t: T) => TypedEvidence(prefix, new Blake2b256().hash(t.immutableBytes))

  object TypePrefixes {
    final val VerificationKeysCurve25519: Byte = 1
    final val VerificationKeysEd25519: Byte = 2
    final val VerificationKeysExtendedEd25519: Byte = 3
    final val VerificationKeysVrfEd25519: Byte = 4
    final val VerificationKeysKesSum: Byte = 5
    final val VerificationKeysKesProduct: Byte = 6

    final val PropositionsPermanentlyLocked: Byte = 7
    final val PropositionsKnowledgeCurve25519: Byte = 8
    final val PropositionsKnowledgeEd25519: Byte = 9
    final val PropositionsKnowledgeExtendedEd25519: Byte = 10
    final val PropositionsKnowledgeHashLock: Byte = 11
    final val PropositionsCompositionalThreshold: Byte = 12
    final val PropositionsCompositionalAnd: Byte = 13
    final val PropositionsCompositionalOr: Byte = 14
    final val PropositionsCompositionalNot: Byte = 15
    final val PropositionsContextualHeightLock: Byte = 16
    final val PropositionsContextualRequiredBoxState: Byte = 17
    final val PropositionsScriptJS: Byte = 18

    final val Ratio: Byte = 19
  }

  trait VerificationKeyInstances {

    implicit val curve25519VKContainsEvidence: ContainsEvidence[VerificationKeys.Curve25519] =
      fromImmutableCodec(TypePrefixes.VerificationKeysCurve25519)

    implicit val ed25519VKContainsEvidence: ContainsEvidence[VerificationKeys.Ed25519] =
      fromImmutableCodec(TypePrefixes.VerificationKeysEd25519)

    implicit val extended25519VKContainsEvidence: ContainsEvidence[VerificationKeys.ExtendedEd25519] =
      fromImmutableCodec(TypePrefixes.VerificationKeysExtendedEd25519)

    implicit val vrfEd25519VKContainsEvidence: ContainsEvidence[VerificationKeys.VrfEd25519] =
      fromImmutableCodec(TypePrefixes.VerificationKeysVrfEd25519)

    implicit val kesSumVKContainsEvidence: ContainsEvidence[VerificationKeys.KesSum] =
      fromImmutableCodec(TypePrefixes.VerificationKeysKesSum)

    implicit val kesProductVKContainsEvidence: ContainsEvidence[VerificationKeys.KesProduct] =
      fromImmutableCodec(TypePrefixes.VerificationKeysKesProduct)

    implicit val vkContainsEvidence: ContainsEvidence[VerificationKey] = {
      case t: VerificationKeys.Curve25519      => curve25519VKContainsEvidence.typedEvidenceOf(t)
      case t: VerificationKeys.Ed25519         => ed25519VKContainsEvidence.typedEvidenceOf(t)
      case t: VerificationKeys.ExtendedEd25519 => extended25519VKContainsEvidence.typedEvidenceOf(t)
      case t: VerificationKeys.VrfEd25519      => vrfEd25519VKContainsEvidence.typedEvidenceOf(t)
      case t: VerificationKeys.KesSum          => kesSumVKContainsEvidence.typedEvidenceOf(t)
      case t: VerificationKeys.KesProduct      => kesProductVKContainsEvidence.typedEvidenceOf(t)
    }
  }

  trait PropositionInstances {

    implicit val permanentlyLockedContainsEvidence: ContainsEvidence[Propositions.PermanentlyLocked.type] =
      fromImmutableCodec(TypePrefixes.PropositionsPermanentlyLocked)

    implicit val curve25519KnowledgePropositionContainsEvidence: ContainsEvidence[Propositions.Knowledge.Curve25519] =
      fromImmutableCodec(TypePrefixes.PropositionsKnowledgeCurve25519)

    implicit val ed25519KnowledgePropositionContainsEvidence: ContainsEvidence[Propositions.Knowledge.Ed25519] =
      fromImmutableCodec(TypePrefixes.PropositionsKnowledgeEd25519)

    implicit val extendedEd25519KnowledgePropositionContainsEvidence
      : ContainsEvidence[Propositions.Knowledge.ExtendedEd25519] =
      fromImmutableCodec(TypePrefixes.PropositionsKnowledgeExtendedEd25519)

    implicit val commitRevealContainsEvidence: ContainsEvidence[Propositions.Knowledge.HashLock] =
      fromImmutableCodec(TypePrefixes.PropositionsKnowledgeHashLock)

    implicit val thresholdContainsEvidence: ContainsEvidence[Propositions.Compositional.Threshold] =
      fromImmutableCodec(TypePrefixes.PropositionsCompositionalThreshold)

    implicit val andContainsEvidence: ContainsEvidence[Propositions.Compositional.And] =
      fromImmutableCodec(TypePrefixes.PropositionsCompositionalAnd)

    implicit val orContainsEvidence: ContainsEvidence[Propositions.Compositional.Or] =
      fromImmutableCodec(TypePrefixes.PropositionsCompositionalOr)

    implicit val notContainsEvidence: ContainsEvidence[Propositions.Compositional.Not] =
      fromImmutableCodec(TypePrefixes.PropositionsCompositionalNot)

    implicit val heightLockContainsEvidence: ContainsEvidence[Propositions.Contextual.HeightLock] =
      fromImmutableCodec(TypePrefixes.PropositionsContextualHeightLock)

    implicit val requiredInputBoxStateContainsEvidence: ContainsEvidence[Propositions.Contextual.RequiredBoxState] =
      fromImmutableCodec(TypePrefixes.PropositionsContextualRequiredBoxState)

    implicit val jsScriptPropositionContainsEvidence: ContainsEvidence[Propositions.Script.JS] =
      fromImmutableCodec(TypePrefixes.PropositionsScriptJS)

    implicit lazy val propositionContainsEvidence: ContainsEvidence[Proposition] = {
      case Propositions.PermanentlyLocked =>
        permanentlyLockedContainsEvidence.typedEvidenceOf(Propositions.PermanentlyLocked)
      case t: Propositions.Knowledge.Curve25519 => curve25519KnowledgePropositionContainsEvidence.typedEvidenceOf(t)
      case t: Propositions.Knowledge.Ed25519    => ed25519KnowledgePropositionContainsEvidence.typedEvidenceOf(t)
      case t: Propositions.Knowledge.ExtendedEd25519 =>
        extendedEd25519KnowledgePropositionContainsEvidence.typedEvidenceOf(t)
      case t: Propositions.Compositional.And           => andContainsEvidence.typedEvidenceOf(t)
      case t: Propositions.Compositional.Or            => orContainsEvidence.typedEvidenceOf(t)
      case t: Propositions.Compositional.Not           => notContainsEvidence.typedEvidenceOf(t)
      case t: Propositions.Compositional.Threshold     => thresholdContainsEvidence.typedEvidenceOf(t)
      case t: Propositions.Contextual.HeightLock       => heightLockContainsEvidence.typedEvidenceOf(t)
      case t: Propositions.Script.JS                   => jsScriptPropositionContainsEvidence.typedEvidenceOf(t)
      case t: Propositions.Knowledge.HashLock          => commitRevealContainsEvidence.typedEvidenceOf(t)
      case t: Propositions.Contextual.RequiredBoxState => requiredInputBoxStateContainsEvidence.typedEvidenceOf(t)
    }
  }

  trait Instances extends VerificationKeyInstances with PropositionInstances {

    implicit val ratioContainsEvidence: ContainsEvidence[Ratio] =
      fromImmutableCodec(TypePrefixes.Ratio)

  }

  object instances extends Instances
}
