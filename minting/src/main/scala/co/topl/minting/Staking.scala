package co.topl.minting

import cats.data.OptionT
import cats.effect.Sync
import cats.implicits._
import cats.Applicative
import co.topl.algebras.{ClockAlgebra, UnsafeResource}
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.consensus.algebras.{ConsensusValidationStateAlgebra, EtaCalculationAlgebra}
import co.topl.crypto.signing.Ed25519
import co.topl.minting.algebras.LeaderElectionMintingAlgebra.VrfHit
import co.topl.minting.algebras._
import co.topl.models._
import co.topl.models.utility.HasLength.instances.bytesLength
import co.topl.models.utility.Sized
import co.topl.typeclasses.implicits._
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.log4cats.{Logger, SelfAwareStructuredLogger}

object Staking {

  object Eval {

    def make[F[_]: Sync](
      a:               StakingAddresses.Operator,
      leaderElection:  LeaderElectionMintingAlgebra[F],
      evolver:         OperationalKeysAlgebra[F],
      consensusState:  ConsensusValidationStateAlgebra[F],
      etaCalculation:  EtaCalculationAlgebra[F],
      ed25519Resource: UnsafeResource[F, Ed25519],
      vrfProof:        VrfProofAlgebra[F],
      clock:           ClockAlgebra[F]
    ): StakingAlgebra[F] = new StakingAlgebra[F] {
      implicit private val logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromClass[F](Staking.getClass)
      val address: F[StakingAddresses.Operator] = a.pure[F]

      def elect(parentSlotId: SlotId, slot: Slot): F[Option[VrfHit]] =
        for {
          slotsPerEpoch <- clock.slotsPerEpoch
          eta           <- etaCalculation.etaToBe(parentSlotId, slot)
          _ <- Applicative[F].whenA(slot % slotsPerEpoch === 0L)(
            vrfProof.precomputeForEpoch(slot / slotsPerEpoch, eta)
          )
          maybeHit <- OptionT(consensusState.operatorRelativeStake(parentSlotId.blockId, slot)(a))
            .flatMapF(relativeStake => leaderElection.getHit(relativeStake, slot, slot - parentSlotId.slot, eta))
            .value
          _ <- Logger[F].debug(
            show"Eligibility at" +
            show" slot=$slot" +
            show" parentId=${parentSlotId.blockId}" +
            show" parentSlot=${parentSlotId.slot}" +
            show" eligible=${maybeHit.nonEmpty}"
          )
        } yield maybeHit

      def certifyBlock(
        parentSlotId:         SlotId,
        slot:                 Slot,
        unsignedBlockBuilder: BlockHeader.Unsigned.PartialOperationalCertificate => Block.Unsigned
      ): F[Option[Block]] =
        OptionT(evolver.operationalKeyForSlot(slot, parentSlotId)).semiflatMap { operationalKeyOut =>
          ed25519Resource.use { ed25519 =>
            val partialCertificate = BlockHeader.Unsigned.PartialOperationalCertificate(
              operationalKeyOut.parentVK,
              operationalKeyOut.parentSignature,
              VerificationKeys.Ed25519(
                Sized.strictUnsafe(ed25519.getVerificationKey(operationalKeyOut.childSK.bytes.data))
              )
            )
            val unsignedBlock = unsignedBlockBuilder(partialCertificate)
            val operationalCertificate = OperationalCertificate(
              operationalKeyOut.parentVK,
              operationalKeyOut.parentSignature,
              partialCertificate.childVK,
              Proofs.Knowledge.Ed25519(
                Sized.strictUnsafe(
                  ed25519.sign(operationalKeyOut.childSK.bytes.data, unsignedBlock.unsignedHeader.signableBytes)
                )
              )
            )
            val header = BlockHeader(
              unsignedBlock.unsignedHeader.parentHeaderId,
              unsignedBlock.unsignedHeader.parentSlot,
              unsignedBlock.unsignedHeader.txRoot,
              unsignedBlock.unsignedHeader.bloomFilter,
              unsignedBlock.unsignedHeader.timestamp,
              unsignedBlock.unsignedHeader.height,
              unsignedBlock.unsignedHeader.slot,
              unsignedBlock.unsignedHeader.eligibilityCertificate,
              operationalCertificate,
              unsignedBlock.unsignedHeader.metadata,
              unsignedBlock.unsignedHeader.address
            )
            Block(
              header,
              unsignedBlock.body
            ).pure[F]
          }
        }.value
    }
  }

}
