package co.topl.consensus.interpreters

import cats.data._
import cats.effect.kernel.Sync
import cats.implicits._
import co.topl.algebras.{ClockAlgebra, Store, UnsafeResource}
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.consensus.algebras._
import co.topl.consensus.models.{
  BlockHeader => ConsensusBlockHeader,
  BlockHeaderValidationFailure,
  BlockHeaderValidationFailures,
  SignatureKesProduct,
  SignatureVrfEd25519,
  VerificationKeyKesProduct,
  VrfArgument
}
import co.topl.crypto.signing.{Ed25519VRF, KesProduct}
import co.topl.crypto.hash.Blake2b256
import co.topl.crypto.signing.Ed25519
import co.topl.models._
import co.topl.models.utility.HasLength.instances.bytesLength
import co.topl.models.utility.{Ratio, Sized}
import co.topl.typeclasses.ContainsEvidence
import co.topl.typeclasses.implicits._
import com.google.common.primitives.Longs
import scalacache.caffeine.CaffeineCache
import scodec.bits.ByteVector

/**
 * Interpreters for the ConsensusValidationAlgebra
 */
object BlockHeaderValidation {

  def make[F[_]: Sync](
    etaInterpreter:           EtaCalculationAlgebra[F],
    consensusValidationState: ConsensusValidationStateAlgebra[F],
    leaderElection:           LeaderElectionValidationAlgebra[F],
    clockAlgebra:             ClockAlgebra[F],
    ed25519VRFResource:       UnsafeResource[F, Ed25519VRF],
    kesProductResource:       UnsafeResource[F, KesProduct],
    ed25519Resource:          UnsafeResource[F, Ed25519],
    blake2b256Resource:       UnsafeResource[F, Blake2b256]
  ): F[BlockHeaderValidationAlgebra[F]] =
    Sync[F].delay(
      new Impl[F](
        etaInterpreter,
        consensusValidationState,
        leaderElection,
        clockAlgebra,
        ed25519VRFResource,
        kesProductResource,
        ed25519Resource,
        blake2b256Resource
      )
    )

  private class Impl[F[_]: Sync](
    etaInterpreter:           EtaCalculationAlgebra[F],
    consensusValidationState: ConsensusValidationStateAlgebra[F],
    leaderElection:           LeaderElectionValidationAlgebra[F],
    clockAlgebra:             ClockAlgebra[F],
    ed25519VRFResource:       UnsafeResource[F, Ed25519VRF],
    kesProductResource:       UnsafeResource[F, KesProduct],
    ed25519Resource:          UnsafeResource[F, Ed25519],
    blake2b256Resource:       UnsafeResource[F, Blake2b256]
  ) extends BlockHeaderValidationAlgebra[F] {

    def validate(
      child:  ConsensusBlockHeader,
      parent: ConsensusBlockHeader
    ): F[Either[BlockHeaderValidationFailure, ConsensusBlockHeader]] = {
      for {
        _         <- statelessVerification(child, parent)
        _         <- EitherT(timeSlotVerification(child))
        _         <- vrfVerification(child)
        _         <- kesVerification(child)
        _         <- registrationVerification(child)
        threshold <- EitherT.liftF(vrfThresholdFor(child, parent))
        _         <- vrfThresholdVerification(child, threshold)
        _         <- eligibilityVerification(child, threshold)
      } yield child
    }.value

    private[consensus] def statelessVerification(child: ConsensusBlockHeader, parent: ConsensusBlockHeader) =
      EitherT
        .pure[F, BlockHeaderValidationFailure](child)
        .ensure(BlockHeaderValidationFailures.NonForwardSlot(child.slot, parent.slot))(child =>
          child.slot > parent.slot
        )
        .ensureOr(child => BlockHeaderValidationFailures.NonForwardTimestamp(child.timestamp, parent.timestamp))(
          child => child.timestamp > parent.timestamp
        )
        .ensureOr(child =>
          BlockHeaderValidationFailures
            .ParentMismatch(TypedBytes.headerFromProtobufString(child.parentHeaderId), parent.id)
        )(id => TypedBytes.headerFromProtobufString(id.parentHeaderId) === parent.id)
        .ensureOr(child => BlockHeaderValidationFailures.NonForwardHeight(child.height, parent.height))(
          _.height === parent.height + 1
        )

    private[consensus] def timeSlotVerification(header: ConsensusBlockHeader) =
      for {
        globalSlot              <- clockAlgebra.globalSlot
        childSlotFromTimestamp  <- clockAlgebra.timestampToSlot(header.timestamp)
        forwardBiasedSlotWindow <- clockAlgebra.forwardBiasedSlotWindow
      } yield Either
        .right[BlockHeaderValidationFailure, ConsensusBlockHeader](header)
        .ensureOr(child => BlockHeaderValidationFailures.TimestampSlotMismatch(child.slot, child.timestamp))(child =>
          childSlotFromTimestamp === child.slot
        )
        .ensureOr(child => BlockHeaderValidationFailures.SlotBeyondForwardBiasedSlotWindow(globalSlot, child.slot))(
          child => child.slot < globalSlot + forwardBiasedSlotWindow
        )

    /**
     * Verifies the given block's VRF certificate syntactic integrity for a particular stateful nonce
     */
    private[consensus] def vrfVerification(
      header: ConsensusBlockHeader
    ): EitherT[F, BlockHeaderValidationFailure, ConsensusBlockHeader] =
      EitherT
        .liftF(
          etaInterpreter.etaToBe(
            SlotId(header.parentSlot, TypedBytes.headerFromProtobufString(header.parentHeaderId)),
            header.slot
          )
        )
        .flatMap { expectedEta => // TODO, create for comprehension, and create validation for empty vrfSig, and vrfVK
          EitherT
            .fromOptionF(
              header.eligibilityCertificate.pure[F],
              BlockHeaderValidationFailures.EmptyEligibilityCertificate
            )
            .flatMap { eligibilityCertificate =>
              val eta = Sized.strictUnsafe[ByteVector, Eta.Length](
                ByteVector(eligibilityCertificate.eta.toByteArray)
              )
              EitherT
                .cond[F](
                  eta === expectedEta,
                  header,
                  BlockHeaderValidationFailures
                    .InvalidEligibilityCertificateEta(eta, expectedEta)
                )
                .flatMap(_ =>
                  EitherT(
                    ed25519VRFResource
                      .use { implicit ed25519vrf =>
                        ed25519vrf
                          .verify(
                            ByteVector(eligibilityCertificate.vrfSig.map(_.value.toByteArray).getOrElse(Array.empty)),
                            VrfArgument(expectedEta, header.slot).signableBytes,
                            ByteVector(eligibilityCertificate.vrfVK.map(_.value.toByteArray).getOrElse(Array.empty))
                          )
                          .pure[F]
                      }
                      .map(
                        Either.cond(
                          _,
                          header,
                          BlockHeaderValidationFailures
                            .InvalidEligibilityCertificateProof(
                              eligibilityCertificate.vrfSig.getOrElse(SignatureVrfEd25519.defaultInstance)
                            )
                        )
                      )
                  )
                )
            }
        }

    /**
     * Verifies the given block's Operational Certificate's parent -> linear commitment, and the Operational
     * Certificate's block signature
     */
    private[consensus] def kesVerification(
      header: ConsensusBlockHeader
    ): EitherT[F, BlockHeaderValidationFailure, ConsensusBlockHeader] =
      EitherT
        .fromOptionF(
          header.operationalCertificate.pure[F],
          BlockHeaderValidationFailures.EmptyOperationalCertificate
        )
        .flatMap { operationalCertificate =>
          EitherT(
            kesProductResource
              .use(kesProduct =>
                kesProduct
                  .verify(
                    operationalCertificate.parentSignature.getOrElse(SignatureKesProduct.defaultInstance),
                    Bytes(operationalCertificate.childVK.map(_.value.toByteArray).getOrElse(Array.empty)) ++ Bytes(
                      Longs.toByteArray(header.slot)
                    ),
                    operationalCertificate.parentVK.getOrElse(VerificationKeyKesProduct.defaultInstance)
                  )
                  .pure[F]
              )
              .map(
                Either.cond(
                  _,
                  header,
                  BlockHeaderValidationFailures.InvalidOperationalParentSignature(
                    operationalCertificate
                  ): BlockHeaderValidationFailure
                )
              )
          )
            .flatMap(_ =>
              EitherT(
                ed25519Resource
                  .use(ed25519 =>
                    // Use the ed25519 instance to verify the childSignature against the header's bytes
                    ed25519
                      .verify(
                        Bytes(operationalCertificate.childSignature.map(_.toByteArray).getOrElse(Array.empty)),
                        header.signableBytes,
                        Bytes(operationalCertificate.childVK.map(_.toByteArray).getOrElse(Array.empty))
                      )
                      .pure[F]
                  )
                  .map(isValid =>
                    // Verification from the previous step returns a boolean, so now check the boolean verification result
                    if (isValid) {
                      // If the verification was valid, just return Right(header)
                      header.asRight[BlockHeaderValidationFailure]
                    } else {
                      // Otherwise, return a Left(InvalidBlockProof)
                      (BlockHeaderValidationFailures.InvalidBlockProof(
                        operationalCertificate
                      ): BlockHeaderValidationFailure).asLeft[ConsensusBlockHeader]
                    }
                  )
              )
            )
        }

    /**
     * Determines the VRF threshold for the given child
     */
    private def vrfThresholdFor(child: ConsensusBlockHeader, parent: ConsensusBlockHeader): F[Ratio] =
      consensusValidationState
        .operatorRelativeStake(child.id, child.slot)(StakingAddresses.operatorFromProtoString(child.address))
        .flatMap(relativeStake =>
          leaderElection.getThreshold(
            relativeStake.getOrElse(Ratio.Zero),
            child.slot - parent.slot
          )
        )

    /**
     * Verify that the threshold evidence stamped on the block matches the threshold generated using local state
     */
    private[consensus] def vrfThresholdVerification(
      header:    ConsensusBlockHeader,
      threshold: Ratio
    ): EitherT[F, BlockHeaderValidationFailure, ConsensusBlockHeader] =
      for {
        eligibilityCertificate <- EitherT.fromOptionF(
          header.eligibilityCertificate.pure[F],
          BlockHeaderValidationFailures.EmptyEligibilityCertificate: BlockHeaderValidationFailure
        )
        res <- EitherT.cond[F](
          ContainsEvidence
            .ratioEvidenceFromProtobufString(eligibilityCertificate.thresholdEvidence)
            .evidence === threshold.typedEvidence.evidence,
          header,
          BlockHeaderValidationFailures.InvalidVrfThreshold(threshold): BlockHeaderValidationFailure
        )
      } yield res

    /**
     * Verify that the block's staker is eligible using their relative stake distribution
     */
    private[consensus] def eligibilityVerification(
      header:    ConsensusBlockHeader,
      threshold: Ratio
    ): EitherT[F, BlockHeaderValidationFailure, ConsensusBlockHeader] =
      EitherT
        .fromOptionF(header.eligibilityCertificate.pure[F], BlockHeaderValidationFailures.EmptyEligibilityCertificate)
        .flatMap { eligibilityCertificate =>
          EitherT
            .liftF(
              ed25519VRFResource
                .use { implicit ed25519Vrf =>
                  ed25519Vrf
                    .proofToHash(
                      Bytes(eligibilityCertificate.getVrfSig.toByteArray)
                    )
                    .pure[F]
                }
                .map(rhoBytes => Rho(Sized.strictUnsafe(rhoBytes)))
                .flatMap(leaderElection.isSlotLeaderForThreshold(threshold))
            )
            .ensure(
              BlockHeaderValidationFailures
                .IneligibleCertificate(threshold, eligibilityCertificate): BlockHeaderValidationFailure
            )(
              identity
            )
            .map(_ => header)
        }

    /**
     * Verifies the staker's registration.  First checks that the staker is registered at all.  Once retrieved,
     * the registration contains a commitment/proof that must be verified using
     * the 0th timestep of the header's operational certificate's "parentVK".  The proof's message is the hash of
     * (the staker's vrfVK concatenated with the staker's poolVK).
     */
    private[consensus] def registrationVerification(
      header: ConsensusBlockHeader
    ): EitherT[F, BlockHeaderValidationFailure, ConsensusBlockHeader] =
      (for {
        eligibilityCertificate <- EitherT.fromOptionF(
          header.eligibilityCertificate.pure[F],
          BlockHeaderValidationFailures.EmptyEligibilityCertificate: BlockHeaderValidationFailure
        )
        operationalCertificate <- EitherT.fromOptionF(
          header.operationalCertificate.pure[F],
          BlockHeaderValidationFailures.EmptyOperationalCertificate: BlockHeaderValidationFailure
        )
      } yield (eligibilityCertificate, operationalCertificate)).flatMap {
        case (eligibilityCertificate, operationalCertificate) =>
          val address = StakingAddresses.operatorFromProtoString(header.address)
          OptionT(consensusValidationState.operatorRegistration(header.id, header.slot)(address))
            .map(_.toConsensusModel.vrfCommitment)
            .toRight(BlockHeaderValidationFailures.Unregistered(address): BlockHeaderValidationFailure)
            .flatMapF(commitment =>
              for {
                message <- blake2b256Resource
                  .use(
                    _.hash(
                      Bytes(eligibilityCertificate.getVrfVK.toByteArray),
                      StakingAddresses.operatorFromProtoString(header.address).vk.bytes.data
                    ).pure[F]
                  )
                isValid <- kesProductResource
                  .use(p => p.verify(commitment, message, operationalCertificate.getParentVK.copy(step = 0)).pure[F])
              } yield Either.cond(
                isValid,
                header,
                BlockHeaderValidationFailures
                  .RegistrationCommitmentMismatch(
                    commitment,
                    eligibilityCertificate.getVrfVK,
                    address.vk
                  )
              )
            )
      }

  }

  object WithCache {

    def make[F[_]: Sync](
      underlying:       BlockHeaderValidationAlgebra[F],
      blockHeaderStore: Store[F, TypedIdentifier, ConsensusBlockHeader]
    ): F[BlockHeaderValidationAlgebra[F]] =
      CaffeineCache[F, Bytes, TypedIdentifier].map(implicit cache =>
        new BlockHeaderValidationAlgebra[F] {

          def validate(
            child:  ConsensusBlockHeader,
            parent: ConsensusBlockHeader
          ): F[Either[BlockHeaderValidationFailure, ConsensusBlockHeader]] =
            OptionT(cache.get(child.id.asTypedBytes.allBytes))
              .map(_ => child.asRight[BlockHeaderValidationFailure])
              .getOrElseF(
                validateParent(parent)
                  .flatMapF(_ => underlying.validate(child, parent))
                  .semiflatTap(h => cache.put(h.id.asTypedBytes.allBytes)(h.id.asTypedBytes))
                  .value
              )

          private def validateParent(
            parent: ConsensusBlockHeader
          ): EitherT[F, BlockHeaderValidationFailure, ConsensusBlockHeader] =
            if (parent.parentSlot < 0)
              // TODO: Is this a security concern?
              // Could an adversary just "claim" the parentSlot is -1 to circumvent validation?
              EitherT.pure[F, BlockHeaderValidationFailure](parent)
            else
              EitherT(
                OptionT(blockHeaderStore.get(TypedBytes.headerFromProtobufString(parent.parentHeaderId)))
                  .getOrElseF(
                    new IllegalStateException(s"Non-existent block header id=${parent.parentHeaderId}")
                      .raiseError[F, ConsensusBlockHeader]
                  )
                  .flatMap(grandParent => Sync[F].defer(validate(parent, grandParent)))
              )
        }
      )
  }
}
