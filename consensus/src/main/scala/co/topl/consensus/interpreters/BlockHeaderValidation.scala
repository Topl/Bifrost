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
          threshold <- vrfThresholdFor(header, parent)
          _         <- vrfThresholdVerification(header, threshold, blake2b256Resource)
          _         <- eligibilityVerification(header, threshold)
        } yield header
    }.value

    private[consensus] def statelessVerification(child: BlockHeader, parent: BlockHeader) =
      for {
        _ <- EitherT
          .cond[F](child.slot > parent.slot, (), BlockHeaderValidationFailures.NonForwardSlot(child.slot, parent.slot))
        _ <- EitherT.cond[F](
          child.timestamp > parent.timestamp,
          (),
          BlockHeaderValidationFailures.NonForwardTimestamp(child.timestamp, parent.timestamp)
        )
        _ <- EitherT
          .cond[F](
            child.height === parent.height + 1,
            (),
            BlockHeaderValidationFailures.NonForwardHeight(child.height, parent.height)
          )
          .leftWiden[BlockHeaderValidationFailure]
      } yield child

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
      for {
        expectedEta <- EitherT.liftF(
          etaInterpreter.etaToBe(
            SlotId(header.parentSlot, header.parentHeaderId),
            header.slot
          )
        )
        eta = Sized.strictUnsafe(header.eligibilityCertificate.eta): Eta
        _ <- EitherT.cond[F](
          eta === expectedEta,
          (),
          BlockHeaderValidationFailures.InvalidEligibilityCertificateEta(eta, expectedEta)
        )
        signatureVerificationResult <- EitherT.liftF(
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
        )
        _ <- EitherT
          .cond[F](
            signatureVerificationResult,
            (),
            BlockHeaderValidationFailures.InvalidEligibilityCertificateProof(header.eligibilityCertificate.vrfSig)
          )
          .leftWiden[BlockHeaderValidationFailure]
      } yield header

    /**
     * Verifies the given block's Operational Certificate's parent -> linear commitment, and the Operational
     * Certificate's block signature
     */
    private[consensus] def kesVerification(
      header: BlockHeader
    ): EitherT[F, BlockHeaderValidationFailure, BlockHeader] =
      for {
        parentCommitmentResult <- EitherT.liftF(
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
        )
        _ <- EitherT.cond[F](
          parentCommitmentResult,
          (),
          BlockHeaderValidationFailures.InvalidOperationalParentSignature(header.operationalCertificate)
        )
        childSignatureResult <- EitherT.liftF(
          ed25519Resource
            .use(ed25519 =>
              // Use the ed25519 instance to verify the childSignature against the header's bytes
              Sync[F].delay(
                ed25519
                  .verify(
                    header.operationalCertificate.childSignature.toByteArray,
                    header.unsigned.signableBytes.toByteArray,
                    Ed25519.PublicKey(header.operationalCertificate.childVK.toByteArray)
                  )
              )
            )
        )
        _ <- EitherT
          .cond[F](
            childSignatureResult,
            (),
            BlockHeaderValidationFailures.InvalidBlockProof(header.operationalCertificate)
          )
          .leftWiden[BlockHeaderValidationFailure]
      } yield header

    /**
     * Determines the VRF threshold for the given child
     */
    private def vrfThresholdFor(
      child:  BlockHeader,
      parent: BlockHeader
    ): EitherT[F, BlockHeaderValidationFailure, Ratio] =
      EitherT
        .fromOptionF(
          consensusValidationState.operatorRelativeStake(child.id, child.slot)(child.address),
          BlockHeaderValidationFailures.Unregistered(child.address)
        )
        .leftWiden[BlockHeaderValidationFailure]
        .semiflatMap(leaderElection.getThreshold(_, child.slot - parent.slot))

    /**
     * Verify that the threshold evidence stamped on the block matches the threshold generated using local state
     */
    private[consensus] def vrfThresholdVerification(
      header:             BlockHeader,
      threshold:          Ratio,
      blake2b256Resource: UnsafeResource[F, Blake2b256]
    ): EitherT[F, BlockHeaderValidationFailure, BlockHeader] =
      for {
        evidence <-
          EitherT
            .liftF[F, BlockHeaderValidationFailure, ByteString](
              blake2b256Resource.use(b => Sync[F].delay(thresholdEvidence(threshold)(b)))
            )
        _ <-
          EitherT.cond[F](
            header.eligibilityCertificate.thresholdEvidence === evidence,
            header,
            BlockHeaderValidationFailures.InvalidVrfThreshold(threshold): BlockHeaderValidationFailure
          )
      } yield header

    /**
     * Verify that the block's staker is eligible using their relative stake distribution
     */
    private[consensus] def eligibilityVerification(
      header:    BlockHeader,
      threshold: Ratio
    ): EitherT[F, BlockHeaderValidationFailure, BlockHeader] =
      for {
        proofHashBytes <- EitherT
          .liftF(
            ed25519VRFResource
              .use(ed25519Vrf =>
                Sync[F].delay(
                  ed25519Vrf.proofToHash(header.eligibilityCertificate.vrfSig.toByteArray)
                )
              )
          )
        rho = Rho(Sized.strictUnsafe(ByteString.copyFrom(proofHashBytes)))
        isSlotLeader <- EitherT.liftF(leaderElection.isSlotLeaderForThreshold(threshold)(rho))
        _ <- EitherT
          .cond[F](
            isSlotLeader,
            (),
            BlockHeaderValidationFailures.IneligibleCertificate(threshold, header.eligibilityCertificate)
          )
          .leftWiden[BlockHeaderValidationFailure]
      } yield header

    /**
     * Verifies the staker's registration.  First checks that the staker is registered at all.  Once retrieved,
     * the registration contains a commitment/proof that must be verified using
     * the 0th timestep of the header's operational certificate's "parentVK".  The proof's message is the hash of
     * (the staker's vrfVK concatenated with the staker's poolVK).
     */
    private[consensus] def registrationVerification(
      header: BlockHeader
    ): EitherT[F, BlockHeaderValidationFailure, BlockHeader] =
      for {
        commitment <-
          EitherT.fromOptionF(
            consensusValidationState.operatorRegistration(header.id, header.slot)(header.address),
            BlockHeaderValidationFailures.Unregistered(header.address)
          )
        message <- EitherT.liftF(
          blake2b256Resource
            .use(b =>
              Sync[F].delay(
                b.hash(header.eligibilityCertificate.vrfVK, header.address.value)
              )
            )
        )
        isValid <- EitherT.liftF(
          kesProductResource
            .use(p =>
              Sync[F].delay(
                p.verify(commitment, message.toArray, header.operationalCertificate.parentVK.copy(step = 0))
              )
            )
        )
        _ <- EitherT
          .cond[F](
            isValid,
            (),
            BlockHeaderValidationFailures
              .RegistrationCommitmentMismatch(commitment, header.eligibilityCertificate.vrfVK, header.address)
          )
          .leftWiden[BlockHeaderValidationFailure]
      } yield header
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
