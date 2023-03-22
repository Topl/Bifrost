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
    blockHeaderStore:         Store[F, BlockId, BlockHeader],
    bigBangBlockId:           BlockId,
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
        blockHeaderStore,
        bigBangBlockId,
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
    blockHeaderStore:         Store[F, BlockId, BlockHeader],
    bigBangBlockId:           BlockId,
    ed25519VRFResource:       UnsafeResource[F, Ed25519VRF],
    kesProductResource:       UnsafeResource[F, KesProduct],
    ed25519Resource:          UnsafeResource[F, Ed25519],
    blake2b256Resource:       UnsafeResource[F, Blake2b256]
  ) extends BlockHeaderValidationAlgebra[F] {

    def validate(header: BlockHeader): F[Either[BlockHeaderValidationFailure, BlockHeader]] = {
      if (header.id === bigBangBlockId) EitherT.rightT[F, BlockHeaderValidationFailure](header)
      else
        for {
          parent    <- EitherT.liftF(blockHeaderStore.getOrRaise(header.parentHeaderId))
          _         <- statelessVerification(header, parent)
          _         <- EitherT(timeSlotVerification(header))
          _         <- vrfVerification(header)
          _         <- kesVerification(header)
          _         <- registrationVerification(header)
          threshold <- EitherT.liftF(vrfThresholdFor(header, parent))
          _         <- vrfThresholdVerification(header, threshold, blake2b256Resource)
          _         <- eligibilityVerification(header, threshold)
        } yield header
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
              BlockHeaderValidationFailures.InvalidEligibilityCertificateEta(eta, expectedEta)
            )
            .flatMap(_ =>
              EitherT(
                ed25519VRFResource
                  .use(ed25519vrf =>
                    Sync[F].delay(
                      ed25519vrf
                        .verify(
                          header.eligibilityCertificate.vrfSig.toByteArray,
                          VrfArgument(expectedEta, header.slot).signableBytes.toByteArray,
                          header.eligibilityCertificate.vrfVK.toByteArray
                        )
                    )
                  )
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
            Sync[F].delay(
              kesProduct
                .verify(
                  header.operationalCertificate.parentSignature,
                  header.operationalCertificate.childVK
                    .concat(ByteString.copyFrom(Longs.toByteArray(header.slot)))
                    .toByteArray,
                  header.operationalCertificate.parentVK
                )
            )
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
                Sync[F].delay(
                  ed25519
                    .verify(
                      header.operationalCertificate.childSignature,
                      header.unsigned.signableBytes,
                      Ed25519.PublicKey(header.operationalCertificate.childVK.toByteArray)
                    )
                )
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
          blake2b256Resource.use(b => Sync[F].delay(thresholdEvidence(threshold)(b)))
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
            .use(ed25519Vrf =>
              Sync[F].delay(
                ed25519Vrf.proofToHash(header.eligibilityCertificate.vrfSig.toByteArray)
              )
            )
            .map(ByteString.copyFrom)
            .map(rhoBytes => Rho(Sized.strictUnsafe(rhoBytes)))
            .flatMap(leaderElection.isSlotLeaderForThreshold(threshold))
        )
        .ensure(
          BlockHeaderValidationFailures
            .IneligibleCertificate(threshold, header.eligibilityCertificate): BlockHeaderValidationFailure
        )(identity)
        .as(header)

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
              .use(b =>
                Sync[F].delay(
                  b.hash(header.eligibilityCertificate.vrfVK, header.address.value)
                )
              )
            isValid <- kesProductResource
              .use(p =>
                Sync[F].delay(
                  p.verify(commitment, message.toArray, header.operationalCertificate.parentVK.copy(step = 0))
                )
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
      blockHeaderStore: Store[F, BlockId, BlockHeader],
      bigBangBlockId:   BlockId
    ): F[BlockHeaderValidationAlgebra[F]] =
      CaffeineCache[F, BlockId, Unit].map(implicit cache =>
        new BlockHeaderValidationAlgebra[F] {

          def validate(header: BlockHeader): F[Either[BlockHeaderValidationFailure, BlockHeader]] =
            if (header.id === bigBangBlockId) header.asRight[BlockHeaderValidationFailure].pure[F]
            else
              cache
                .cachingF(header.id)(ttl = None)(
                  EitherT
                    .liftF(Sync[F].defer(blockHeaderStore.getOrRaise(header.parentHeaderId)))
                    .flatMapF(validate)
                    .flatMapF(_ => underlying.validate(header))
                    .void
                    .leftMap(new WrappedFailure(_))
                    .rethrowT
                )
                .as(header.asRight[BlockHeaderValidationFailure])
                .recover { case w: WrappedFailure => w.failure.asLeft[BlockHeader] }
        }
      )

    private class WrappedFailure(val failure: BlockHeaderValidationFailure) extends Exception
  }
}
