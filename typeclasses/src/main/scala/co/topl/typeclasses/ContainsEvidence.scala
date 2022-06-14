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

  trait VerificationKeyInstances {

    implicit val curve25519VKContainsEvidence: ContainsEvidence[VerificationKeys.Curve25519] =
      fromImmutableCodec(1)

    implicit val ed25519VKContainsEvidence: ContainsEvidence[VerificationKeys.Ed25519] =
      fromImmutableCodec(2)

    implicit val extended25519VKContainsEvidence: ContainsEvidence[VerificationKeys.ExtendedEd25519] =
      fromImmutableCodec(3)

    implicit val vrfEd25519VKContainsEvidence: ContainsEvidence[VerificationKeys.VrfEd25519] =
      fromImmutableCodec(4)

    implicit val kesSumVKContainsEvidence: ContainsEvidence[VerificationKeys.KesSum] =
      fromImmutableCodec(5)

    implicit val kesProductVKContainsEvidence: ContainsEvidence[VerificationKeys.KesProduct] =
      fromImmutableCodec(6)

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
      fromImmutableCodec(1)

    implicit val curve25519KnowledgePropositionContainsEvidence: ContainsEvidence[Propositions.Knowledge.Curve25519] =
      fromImmutableCodec(2)

    implicit val ed25519KnowledgePropositionContainsEvidence: ContainsEvidence[Propositions.Knowledge.Ed25519] =
      fromImmutableCodec(3)

    implicit val extendedEd25519KnowledgePropositionContainsEvidence
      : ContainsEvidence[Propositions.Knowledge.ExtendedEd25519] =
      fromImmutableCodec(4)

    implicit val commitRevealContainsEvidence: ContainsEvidence[Propositions.Knowledge.HashLock] =
      fromImmutableCodec(5)

    implicit val thresholdContainsEvidence: ContainsEvidence[Propositions.Compositional.Threshold] =
      fromImmutableCodec(6)

    implicit val andContainsEvidence: ContainsEvidence[Propositions.Compositional.And] =
      fromImmutableCodec(7)

    implicit val orContainsEvidence: ContainsEvidence[Propositions.Compositional.Or] =
      fromImmutableCodec(8)

    implicit val notContainsEvidence: ContainsEvidence[Propositions.Compositional.Not] =
      fromImmutableCodec(9)

    implicit val heightLockContainsEvidence: ContainsEvidence[Propositions.Contextual.HeightLock] =
      fromImmutableCodec(10)

    implicit val requiredInputBoxStateContainsEvidence: ContainsEvidence[Propositions.Contextual.RequiredBoxState] =
      fromImmutableCodec(11)

    implicit val jsScriptPropositionContainsEvidence: ContainsEvidence[Propositions.Script.JS] =
      fromImmutableCodec(12)

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
      fromImmutableCodec(10)

  }

  object instances extends Instances
}
