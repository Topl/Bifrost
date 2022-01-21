package co.topl.minting

import cats.{Applicative, Monad}
import cats.data.OptionT
import cats.implicits._
import co.topl.algebras.{ClockAlgebra, UnsafeResource}
import co.topl.consensus.algebras.EtaCalculationAlgebra
import co.topl.crypto.signing.Ed25519
import co.topl.minting.algebras.LeaderElectionMintingAlgebra.VrfHit
import co.topl.minting.algebras._
import co.topl.models._
import co.topl.typeclasses.implicits._
import org.typelevel.log4cats.Logger

object Staking {

  object Eval {

    def make[F[_]: Monad: Logger](
      a:                      TaktikosAddress,
      leaderElection:         LeaderElectionMintingAlgebra[F],
      evolver:                OperationalKeysAlgebra[F],
      vrfRelativeStakeLookup: VrfRelativeStakeMintingLookupAlgebra[F],
      etaCalculation:         EtaCalculationAlgebra[F],
      ed25519Resource:        UnsafeResource[F, Ed25519],
      vrfProof:               VrfProofAlgebra[F],
      clock:                  ClockAlgebra[F]
    ): StakingAlgebra[F] = new StakingAlgebra[F] {
      val address: F[TaktikosAddress] = a.pure[F]

      def elect(parent: BlockHeaderV2, slot: Slot): F[Option[VrfHit]] =
        for {
          _             <- Logger[F].debug(show"Attempting mint at slot=$slot with parent=${parent.id}")
          slotsPerEpoch <- clock.slotsPerEpoch
          eta           <- etaCalculation.etaToBe(parent.slotId, slot)
          _ <- Applicative[F].whenA(slot % slotsPerEpoch === 0L)(
            vrfProof.precomputeForEpoch(slot / slotsPerEpoch, eta)
          )
          maybeHit <- OptionT(vrfRelativeStakeLookup.lookupAt(slot, a))
            .flatMapF(relativeStake => leaderElection.getHit(relativeStake, slot, slot - parent.slot, eta))
            .value
        } yield maybeHit

      def certifyBlock(
        parentSlotId:         SlotId,
        slot:                 Slot,
        unsignedBlockBuilder: BlockHeaderV2.Unsigned.PartialOperationalCertificate => BlockV2.Unsigned
      ): F[Option[BlockV2]] =
        OptionT(evolver.operationalKeyForSlot(slot, parentSlotId)).semiflatMap { operationalKeyOut =>
          ed25519Resource.use { ed25519 =>
            val partialCertificate = BlockHeaderV2.Unsigned.PartialOperationalCertificate(
              operationalKeyOut.parentVK,
              operationalKeyOut.parentSignature,
              ed25519.getVerificationKey(operationalKeyOut.childSK)
            )
            val unsignedBlock = unsignedBlockBuilder(partialCertificate)
            val operationalCertificate = OperationalCertificate(
              operationalKeyOut.parentVK,
              operationalKeyOut.parentSignature,
              ed25519.getVerificationKey(operationalKeyOut.childSK),
              ed25519.sign(operationalKeyOut.childSK, unsignedBlock.unsignedHeader.signableBytes)
            )
            val header = BlockHeaderV2(
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
            BlockV2(
              header,
              BlockBodyV2(header.id, unsignedBlock.transactions)
            )
          }
        }.value
    }
  }

}
