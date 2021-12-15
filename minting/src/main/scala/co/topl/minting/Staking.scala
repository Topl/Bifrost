package co.topl.minting

import cats.Monad
import cats.data.OptionT
import cats.implicits._
import co.topl.consensus.algebras.EtaCalculationAlgebra
import co.topl.crypto.signing.Ed25519
import co.topl.minting.algebras.LeaderElectionMintingAlgebra.VrfHit
import co.topl.minting.algebras._
import co.topl.models._
import co.topl.typeclasses.implicits._

object Staking {

  object Eval {

    def make[F[_]: Monad](
      a:                      TaktikosAddress,
      leaderElection:         LeaderElectionMintingAlgebra[F],
      evolver:                OperationalKeysAlgebra[F],
      vrfRelativeStakeLookup: VrfRelativeStakeMintingLookupAlgebra[F],
      etaCalculation:         EtaCalculationAlgebra[F]
    )(implicit ed25519:       Ed25519): StakingAlgebra[F] = new StakingAlgebra[F] {
      val address: F[TaktikosAddress] = a.pure[F]

      def elect(parent: BlockHeaderV2, slot: Slot): F[Option[VrfHit]] =
        etaCalculation
          .etaToBe(parent.slotId, slot)
          .flatMap(eta =>
            OptionT(vrfRelativeStakeLookup.lookupAt(slot, a))
              .flatMapF(relativeStake => leaderElection.getHit(relativeStake, slot, slot - parent.slot, eta))
              .value
          )

      def certifyBlock(unsignedBlock: BlockV2.Unsigned): F[Option[BlockV2]] = {
        val parentSlotId = SlotId(unsignedBlock.unsignedHeader.parentSlot, unsignedBlock.unsignedHeader.parentHeaderId)
        OptionT(evolver.operationalKeyForSlot(unsignedBlock.unsignedHeader.slot.toInt, parentSlotId))
          .map(evolvedKey =>
            OperationalCertificate(
              ed25519.sign(evolvedKey.sk, unsignedBlock.unsignedHeader.signableBytes),
              ed25519.getVerificationKey(evolvedKey.sk),
              evolvedKey.proofOfVk
            )
          )
          .map(operationalCertificate =>
            BlockHeaderV2(
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
          )
          .map(header =>
            BlockV2(
              header,
              BlockBodyV2(header.id, unsignedBlock.transactions)
            )
          )
          .value
      }
    }
  }

}
