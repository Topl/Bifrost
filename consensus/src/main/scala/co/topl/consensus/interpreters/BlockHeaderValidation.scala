package co.topl.consensus.interpreters

import cats.data._
import cats.effect.kernel.Sync
import cats.implicits._
import co.topl.algebras.{ClockAlgebra, Store, UnsafeResource}
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.consensus.algebras._
import co.topl.consensus.models.BlockId
import co.topl.consensus.models._
import co.topl.consensus.thresholdEvidence
import co.topl.crypto.signing.{Ed25519VRF, KesProduct}
import co.topl.crypto.hash.Blake2b256
import co.topl.crypto.signing.Ed25519
import co.topl.models._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Lengths._
import co.topl.models.utility._
import co.topl.typeclasses.implicits._
import com.google.common.primitives.Longs
import com.google.protobuf.ByteString
import scalacache.caffeine.CaffeineCache

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
      child:  BlockHeader,
      parent: BlockHeader
    ): F[Either[BlockHeaderValidationFailure, BlockHeader]] = {
      for {
        _         <- statelessVerification(child, parent)
        _         <- EitherT(timeSlotVerification(child))
        _         <- vrfVerification(child)
        _         <- kesVerification(child)
        _         <- registrationVerification(child)
        threshold <- EitherT.liftF(vrfThresholdFor(child, parent))
        _         <- vrfThresholdVerification(child, threshold, blake2b256Resource)
        _         <- eligibilityVerification(child, threshold)
      } yield child
    }.value

    private[consensus] def statelessVerification(child: BlockHeader, parent: BlockHeader) =
      EitherT
        .pure[F, BlockHeaderValidationFailure](child)
        .ensure(BlockHeaderValidationFailures.NonForwardSlot(child.slot, parent.slot))(child =>
          child.slot > parent.slot
        )
        .ensureOr(child => BlockHeaderValidationFailures.NonForwardTimestamp(child.timestamp, parent.timestamp))(
          child => child.timestamp > parent.timestamp
        )
        .ensureOr(child => BlockHeaderValidationFailures.ParentMismatch(child.parentHeaderId, parent.id))(child =>
          child.parentHeaderId === parent.id
        )
        .ensureOr(child => BlockHeaderValidationFailures.NonForwardHeight(child.height, parent.height))(
          _.height === parent.height + 1
        )

    private[consensus] def timeSlotVerification(header: BlockHeader) =
      for {
        globalSlot              <- clockAlgebra.globalSlot
        childSlotFromTimestamp  <- clockAlgebra.timestampToSlot(header.timestamp)
        forwardBiasedSlotWindow <- clockAlgebra.forwardBiasedSlotWindow
      } yield Either
        .right[BlockHeaderValidationFailure, BlockHeader](header)
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
      header: BlockHeader
    ): EitherT[F, BlockHeaderValidationFailure, BlockHeader] =
      EitherT
        .liftF(
          etaInterpreter.etaToBe(
            SlotId.of(header.parentSlot, header.parentHeaderId),
            header.slot
          )
        )
        .flatMap { expectedEta =>
          val eta: Eta = Sized.strictUnsafe(header.eligibilityCertificate.eta)
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
                        header.eligibilityCertificate.vrfSig.toByteArray,
                        VrfArgument(expectedEta, header.slot).signableBytes.toByteArray,
                        header.eligibilityCertificate.vrfVK.toByteArray
                      )
                      .pure[F]
                  }
                  .map(
                    Either.cond(
                      _,
                      header,
                      BlockHeaderValidationFailures
                        .InvalidEligibilityCertificateProof(
                          header.eligibilityCertificate.vrfSig
                        )
                    )
                  )
              )
            )

        }

    /**
     * Verifies the given block's Operational Certificate's parent -> linear commitment, and the Operational
     * Certificate's block signature
     */
    private[consensus] def kesVerification(
      header: BlockHeader
    ): EitherT[F, BlockHeaderValidationFailure, BlockHeader] =
      EitherT(
        kesProductResource
          .use(kesProduct =>
            kesProduct
              .verify(
                header.operationalCertificate.parentSignature,
                header.operationalCertificate.childVK
                  .concat(ByteString.copyFrom(Longs.toByteArray(header.slot)))
                  .toByteArray,
                header.operationalCertificate.parentVK
              )
              .pure[F]
          )
          .map(
            Either.cond(
              _,
              header,
              BlockHeaderValidationFailures.InvalidOperationalParentSignature(
                header.operationalCertificate
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
                    header.operationalCertificate.childSignature,
                    header.unsigned.signableBytes,
                    header.operationalCertificate.childVK
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
                    header.operationalCertificate
                  ): BlockHeaderValidationFailure).asLeft[BlockHeader]
                }
              )
          )
        )

    /**
     * Determines the VRF threshold for the given child
     */
    private def vrfThresholdFor(child: BlockHeader, parent: BlockHeader): F[Ratio] =
      consensusValidationState
        .operatorRelativeStake(child.id, child.slot)(child.address)
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
      header:             BlockHeader,
      threshold:          Ratio,
      blake2b256Resource: UnsafeResource[F, Blake2b256]
    ): EitherT[F, BlockHeaderValidationFailure, BlockHeader] =
      EitherT
        .liftF[F, BlockHeaderValidationFailure, ByteString](
          blake2b256Resource.use(thresholdEvidence(threshold)(_).pure[F])
        )
        .flatMap(evidence =>
          EitherT.cond[F](
            header.eligibilityCertificate.thresholdEvidence === evidence,
            header,
            BlockHeaderValidationFailures.InvalidVrfThreshold(threshold): BlockHeaderValidationFailure
          )
        )

    /**
     * Verify that the block's staker is eligible using their relative stake distribution
     */
    private[consensus] def eligibilityVerification(
      header:    BlockHeader,
      threshold: Ratio
    ): EitherT[F, BlockHeaderValidationFailure, BlockHeader] =
      EitherT
        .liftF(
          ed25519VRFResource
            .use { implicit ed25519Vrf =>
              ed25519Vrf
                .proofToHash(header.eligibilityCertificate.vrfSig.toByteArray)
                .pure[F]
            }
            .map(ByteString.copyFrom)
            .map(rhoBytes => Rho(Sized.strictUnsafe(rhoBytes)))
            .flatMap(leaderElection.isSlotLeaderForThreshold(threshold))
        )
        .ensure(
          BlockHeaderValidationFailures
            .IneligibleCertificate(threshold, header.eligibilityCertificate): BlockHeaderValidationFailure
        )(
          identity
        )
        .map(_ => header)

    /**
     * Verifies the staker's registration.  First checks that the staker is registered at all.  Once retrieved,
     * the registration contains a commitment/proof that must be verified using
     * the 0th timestep of the header's operational certificate's "parentVK".  The proof's message is the hash of
     * (the staker's vrfVK concatenated with the staker's poolVK).
     */
    private[consensus] def registrationVerification(
      header: BlockHeader
    ): EitherT[F, BlockHeaderValidationFailure, BlockHeader] =
      OptionT(consensusValidationState.operatorRegistration(header.id, header.slot)(header.address))
        .toRight(BlockHeaderValidationFailures.Unregistered(header.address): BlockHeaderValidationFailure)
        .flatMapF { commitment =>
          for {
            message <- blake2b256Resource
              .use(
                _.hash(header.eligibilityCertificate.vrfVK, header.address.value).pure[F]
              )
            isValid <- kesProductResource
              .use(p =>
                p.verify(commitment, message.toArray, header.operationalCertificate.parentVK.copy(step = 0)).pure[F]
              )
          } yield Either.cond(
            isValid,
            header,
            BlockHeaderValidationFailures
              .RegistrationCommitmentMismatch(
                commitment,
                header.eligibilityCertificate.vrfVK,
                header.address
              )
          )
        }
  }

  object WithCache {

    def make[F[_]: Sync](
      underlying:       BlockHeaderValidationAlgebra[F],
      blockHeaderStore: Store[F, BlockId, BlockHeader]
    ): F[BlockHeaderValidationAlgebra[F]] =
      CaffeineCache[F, BlockId, BlockId].map(implicit cache =>
        new BlockHeaderValidationAlgebra[F] {

          def validate(
            child:  BlockHeader,
            parent: BlockHeader
          ): F[Either[BlockHeaderValidationFailure, BlockHeader]] =
            OptionT(cache.get(child.id))
              .map(_ => child.asRight[BlockHeaderValidationFailure])
              .getOrElseF(
                validateParent(parent)
                  .flatMapF(_ => underlying.validate(child, parent))
                  .semiflatTap(h => cache.put(h.id)(h.id))
                  .value
              )

          private def validateParent(
            parent: BlockHeader
          ): EitherT[F, BlockHeaderValidationFailure, BlockHeader] =
            if (parent.parentSlot < 0)
              // TODO: Is this a security concern?
              // Could an adversary just "claim" the parentSlot is -1 to circumvent validation?
              EitherT.pure[F, BlockHeaderValidationFailure](parent)
            else
              EitherT(
                OptionT(blockHeaderStore.get(parent.parentHeaderId))
                  .getOrElseF(
                    new IllegalStateException(s"Non-existent block header id=${parent.parentHeaderId}")
                      .raiseError[F, BlockHeader]
                  )
                  .flatMap(grandParent => Sync[F].defer(validate(parent, grandParent)))
              )
        }
      )
  }
}
