package co.topl.minting

import cats.Monad
import cats.data.OptionT
import cats.implicits._
import co.topl.crypto.KeyIndexes
import co.topl.minting.algebras.LeaderElectionMintingAlgebra.VrfHit
import co.topl.minting.algebras._
import co.topl.models._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.{Bip32Index, Sized}
import co.topl.typeclasses.implicits._

object Staking {

  object Eval {

    def make[F[_]: Monad](
      a:                      TaktikosAddress,
      leaderElection:         LeaderElectionMintingAlgebra[F],
      evolver:                KeyEvolverAlgebra[KeyIndexes.Bip32, F],
      vrfRelativeStakeLookup: VrfRelativeStakeMintingLookupAlgebra[F],
      etaLookup:              EtaMintingAlgebra[F]
    ): StakingAlgebra[F] = new StakingAlgebra[F] {
      val address: F[TaktikosAddress] = a.pure[F]

      def elect(parent: BlockHeaderV2, slot: Slot): F[Option[VrfHit]] =
        etaLookup
          .etaOf(slot)
          .flatMap(eta =>
            OptionT(vrfRelativeStakeLookup.lookupAt(slot, a))
              .flatMapF(relativeStake => leaderElection.getHit(relativeStake, slot, slot - parent.slot, eta))
              .value
          )

      def certifyBlock(unsignedBlock: BlockV2.Unsigned): F[BlockV2] =
        evolver
          .evolveKey(KeyIndexes.Bip32.Hardened(unsignedBlock.unsignedHeader.slot.toInt))
          .map(evolvedKey => temporaryOpCert)
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

      // TODO: Generate a _real_ operational certificate
      val temporaryOpCert: OperationalCertificate = OperationalCertificate(
        Proofs.Signature.Ed25519(Sized.strictUnsafe(Bytes(Array.fill(32)(0: Byte))))
      )
//
//      private def temporaryOpCert =
//        OperationalCertificate(
//          opSig = Proofs.Signature.HdKes(
//            i = 0,
//            vkI = VerificationKeys.Ed25519(BlockGenesis.zeroBytes),
//            ecSignature = Proofs.Signature.Ed25519(BlockGenesis.zeroBytes),
//            sigSumJ = Proofs.Signature.SumProduct(
//              ecSignature = Proofs.Signature.Ed25519(BlockGenesis.zeroBytes),
//              vkK = VerificationKeys.Ed25519(BlockGenesis.zeroBytes),
//              index = 0,
//              witness = Nil
//            ),
//            sigSumK = Proofs.Signature.SumProduct(
//              ecSignature = Proofs.Signature.Ed25519(BlockGenesis.zeroBytes),
//              vkK = VerificationKeys.Ed25519(BlockGenesis.zeroBytes),
//              index = 0,
//              witness = Nil
//            )
//          ),
//          xvkM =
//            VerificationKeys.ExtendedEd25519(VerificationKeys.Ed25519(BlockGenesis.zeroBytes), BlockGenesis.zeroBytes),
//          slotR = 0
//        )
    }
  }

}
