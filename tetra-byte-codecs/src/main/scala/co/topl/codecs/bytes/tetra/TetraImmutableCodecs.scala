package co.topl.codecs.bytes.tetra

import co.topl.codecs.bytes.scodecs.valuetypes.{intCodec, longCodec}
import co.topl.codecs.bytes.typeclasses.ImmutableCodec
import co.topl.models.BlockHeader.Unsigned.PartialOperationalCertificate
import co.topl.models._
import co.topl.models.utility.Ratio

trait TetraImmutableCodecs {
  import TetraScodecCodecs._

  implicit val typedBytesImmutableCodec: ImmutableCodec[TypedBytes] =
    ImmutableCodec.fromScodecCodec

  implicit val ratioStableCodec: ImmutableCodec[Ratio] =
    ImmutableCodec.fromScodecCodec

  implicit val stakingAddressBaseImmutableCodec: ImmutableCodec[StakingAddresses.Operator] =
    ImmutableCodec.fromScodecCodec

  implicit val stakingAddressImmutableCodec: ImmutableCodec[StakingAddress] =
    ImmutableCodec.fromScodecCodec

  implicit val spendingAddressImmutableCodec: ImmutableCodec[SpendingAddress] =
    ImmutableCodec.fromScodecCodec

  implicit val fullAddressImmutableCodec: ImmutableCodec[FullAddress] =
    ImmutableCodec.fromScodecCodec

  implicit val eligibilityCertificateStableCodec: ImmutableCodec[EligibilityCertificate] =
    ImmutableCodec.fromScodecCodec

  implicit val operationalCertificateStableCodec: ImmutableCodec[OperationalCertificate] =
    ImmutableCodec.fromScodecCodec

  implicit val partialOperationalCertificateStableCodec: ImmutableCodec[PartialOperationalCertificate] =
    ImmutableCodec.fromScodecCodec

  implicit val unsignedHeaderV2StableCodec: ImmutableCodec[BlockHeader.Unsigned] =
    ImmutableCodec.fromScodecCodec

  implicit val transactionStableCodec: ImmutableCodec[Transaction] =
    ImmutableCodec.fromScodecCodec

  implicit val unprovenTransactionStableCodec: ImmutableCodec[Transaction.Unproven] =
    ImmutableCodec.fromScodecCodec

  implicit val curve25519VKImmutableCodec: ImmutableCodec[VerificationKeys.Curve25519] =
    ImmutableCodec.fromScodecCodec

  implicit val ed25519VKImmutableCodec: ImmutableCodec[VerificationKeys.Ed25519] =
    ImmutableCodec.fromScodecCodec

  implicit val extendedEd25519VKStableCodec: ImmutableCodec[VerificationKeys.ExtendedEd25519] =
    ImmutableCodec.fromScodecCodec

  implicit val ed25519VRFVKStableCodec: ImmutableCodec[VerificationKeys.VrfEd25519] =
    ImmutableCodec.fromScodecCodec

  implicit val kesSumVKStableCodec: ImmutableCodec[VerificationKeys.KesSum] =
    ImmutableCodec.fromScodecCodec

  implicit val kesProductVKStableCodec: ImmutableCodec[VerificationKeys.KesProduct] =
    ImmutableCodec.fromScodecCodec

  implicit val propositionsPermanentlyLockedImmutableCodec: ImmutableCodec[Propositions.PermanentlyLocked.type] =
    ImmutableCodec.fromScodecCodec

  implicit val propositionKnowledgeCurve25519ImmutableCodec: ImmutableCodec[Propositions.Knowledge.Curve25519] =
    ImmutableCodec.fromScodecCodec

  implicit val propositionKnowledgeEd25519ImmutableCodec: ImmutableCodec[Propositions.Knowledge.Ed25519] =
    ImmutableCodec.fromScodecCodec

  implicit val propositionKnowledgeExtendedEd25519ImmutableCodec
    : ImmutableCodec[Propositions.Knowledge.ExtendedEd25519] =
    ImmutableCodec.fromScodecCodec

  implicit val propositionKnowledgehashLockImmutableCodec: ImmutableCodec[Propositions.Knowledge.HashLock] =
    ImmutableCodec.fromScodecCodec

  implicit val propositionCompositionalThresholdImmutableCodec: ImmutableCodec[Propositions.Compositional.Threshold] =
    ImmutableCodec.fromScodecCodec

  implicit val propositionCompositionalAndImmutableCodec: ImmutableCodec[Propositions.Compositional.And] =
    ImmutableCodec.fromScodecCodec

  implicit val propositionCompositionalOrImmutableCodec: ImmutableCodec[Propositions.Compositional.Or] =
    ImmutableCodec.fromScodecCodec

  implicit val propositionCompositionalNotImmutableCodec: ImmutableCodec[Propositions.Compositional.Not] =
    ImmutableCodec.fromScodecCodec

  implicit val propositionContextualHeightLockImmutableCodec: ImmutableCodec[Propositions.Contextual.HeightLock] =
    ImmutableCodec.fromScodecCodec

  implicit val propositionContextualRequiredTransactionIOImmutableCodec
    : ImmutableCodec[Propositions.Contextual.RequiredTransactionIO] =
    ImmutableCodec.fromScodecCodec

  implicit val propositionImmutableCodec: ImmutableCodec[Proposition] = ImmutableCodec.fromScodecCodec

  implicit val kesSumProofStableCodec: ImmutableCodec[Proofs.Knowledge.KesSum] =
    ImmutableCodec.fromScodecCodec

  implicit val kesProductProofStableCodec: ImmutableCodec[Proofs.Knowledge.KesProduct] =
    ImmutableCodec.fromScodecCodec

  implicit val proofImmutableCodec: ImmutableCodec[Proof] = ImmutableCodec.fromScodecCodec

  implicit val poolRegistrationBoxImmutableCodec: ImmutableCodec[Box.Values.Registrations.Operator] =
    ImmutableCodec.fromScodecCodec

  implicit val longStableCodec: ImmutableCodec[Long] =
    ImmutableCodec.fromScodecCodec

  implicit val intStableCodec: ImmutableCodec[Int] =
    ImmutableCodec.fromScodecCodec

}

object TetraImmutableCodecs extends TetraImmutableCodecs
