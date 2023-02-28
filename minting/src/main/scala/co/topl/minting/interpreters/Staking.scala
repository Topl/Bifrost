package co.topl.minting.interpreters

import cats.data.OptionT
import cats.effect.Sync
import cats.implicits._
import co.topl.algebras.UnsafeResource
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.consensus.algebras.{
  ConsensusValidationStateAlgebra,
  EtaCalculationAlgebra,
  LeaderElectionValidationAlgebra
}
import co.topl.crypto.signing.Ed25519
import co.topl.minting.algebras._
import co.topl.minting.models.VrfHit
import co.topl.models._
import co.topl.models.utility._
import co.topl.consensus.models.{
  BlockHeader,
  EligibilityCertificate,
  OperationalCertificate,
  SignatureEd25519,
  SlotId,
  VerificationKeyVrfEd25519
}
import co.topl.node.models.Block
import co.topl.typeclasses.implicits._
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.log4cats.{Logger, SelfAwareStructuredLogger}

object Staking {

  def make[F[_]: Sync](
    a:                        StakingAddresses.Operator,
    vkVrf:                    VerificationKeyVrfEd25519,
    operationalKeyMaker:      OperationalKeyMakerAlgebra[F],
    consensusState:           ConsensusValidationStateAlgebra[F],
    etaCalculation:           EtaCalculationAlgebra[F],
    ed25519Resource:          UnsafeResource[F, Ed25519],
    vrfCalculator:            VrfCalculatorAlgebra[F],
    leaderElectionValidation: LeaderElectionValidationAlgebra[F]
  ): StakingAlgebra[F] =
    new StakingAlgebra[F] {
      implicit private val logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromClass[F](Staking.getClass)
      val address: F[StakingAddresses.Operator] = a.pure[F]

      def elect(parentSlotId: SlotId, slot: Slot): F[Option[VrfHit]] =
        for {
          eta <- etaCalculation.etaToBe(parentSlotId, slot)
          maybeHit <- OptionT(consensusState.operatorRelativeStake(parentSlotId.blockId: TypedIdentifier, slot)(a))
            .flatMapF(relativeStake => getHit(relativeStake, slot, slot - parentSlotId.slot, eta))
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
        parentSlotId: SlotId,
        slot:         Slot,
        unsignedBlockBuilder: co.topl.models.BlockHeader.UnsignedConsensus.PartialOperationalCertificate => co.topl.models.Block.Unsigned
      ): F[Option[Block]] =
        OptionT(operationalKeyMaker.operationalKeyForSlot(slot, parentSlotId)).semiflatMap { operationalKeyOut =>
          for {
            partialCertificate <- Sync[F].delay(
              co.topl.models.BlockHeader.UnsignedConsensus.PartialOperationalCertificate(
                operationalKeyOut.parentVK,
                operationalKeyOut.parentSignature,
                operationalKeyOut.childVK
              )
            )
            unsignedBlock = unsignedBlockBuilder(partialCertificate)
            messageToSign = unsignedBlock.unsignedHeader.signableBytes
            signature <- ed25519Resource.use(_.sign(operationalKeyOut.childSK.value, messageToSign).pure[F])
            operationalCertificate = OperationalCertificate(
              operationalKeyOut.parentVK,
              operationalKeyOut.parentSignature,
              partialCertificate.childVK,
              SignatureEd25519.of(signature)
            )
            header = BlockHeader(
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
          } yield Block.of(header, unsignedBlock.body)
        }.value

      def getHit(relativeStake: Ratio, slot: Slot, slotDiff: Long, eta: Eta): F[Option[VrfHit]] =
        for {
          threshold <- leaderElectionValidation.getThreshold(relativeStake, slotDiff)
          testProof <- vrfCalculator.proofForSlot(slot, eta)
          rho       <- vrfCalculator.rhoForSlot(slot, eta)
          isLeader  <- leaderElectionValidation.isSlotLeaderForThreshold(threshold)(rho)
          vrfHit <- OptionT
            .when[F, VrfHit](isLeader)(
              VrfHit(
                EligibilityCertificate(
                  testProof,
                  vkVrf,
                  threshold.typedEvidence.evidence.data,
                  eta.data
                ),
                slot,
                threshold
              )
            )
            .value
        } yield vrfHit

    }
}
