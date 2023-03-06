package co.topl.minting.interpreters

import cats.data.OptionT
import cats.effect.Resource
import cats.effect.Sync
import cats.implicits._
import co.topl.algebras.UnsafeResource
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.consensus.algebras.ConsensusValidationStateAlgebra
import co.topl.consensus.algebras.EtaCalculationAlgebra
import co.topl.consensus.algebras.LeaderElectionValidationAlgebra
import co.topl.consensus.models.BlockHeader
import co.topl.consensus.models.EligibilityCertificate
import co.topl.consensus.models.OperationalCertificate
import co.topl.consensus.models.SlotId
import co.topl.crypto.hash.Blake2b256
import co.topl.crypto.signing.Ed25519
import co.topl.minting.algebras._
import co.topl.minting.models.VrfHit
import co.topl.models._
import co.topl.models.utility._
import co.topl.typeclasses.implicits._
import com.google.protobuf.ByteString
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.SelfAwareStructuredLogger

object Staking {

  def make[F[_]: Sync](
    a:                        StakingAddress,
    vkVrf:                    ByteString,
    operationalKeyMaker:      OperationalKeyMakerAlgebra[F],
    consensusState:           ConsensusValidationStateAlgebra[F],
    etaCalculation:           EtaCalculationAlgebra[F],
    ed25519Resource:          UnsafeResource[F, Ed25519],
    blake2b256Resource:       UnsafeResource[F, Blake2b256],
    vrfCalculator:            VrfCalculatorAlgebra[F],
    leaderElectionValidation: LeaderElectionValidationAlgebra[F]
  ): Resource[F, StakingAlgebra[F]] =
    Resource.pure {
      new StakingAlgebra[F] {
        implicit private val logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromClass[F](Staking.getClass)
        val address: F[StakingAddress] = a.pure[F]

        def elect(parentSlotId: SlotId, slot: Slot): F[Option[VrfHit]] =
          for {
            eta <- etaCalculation.etaToBe(parentSlotId, slot)
            maybeHit <- OptionT(consensusState.operatorRelativeStake(parentSlotId.blockId, slot)(a))
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
          parentSlotId:         SlotId,
          slot:                 Slot,
          unsignedBlockBuilder: UnsignedBlockHeader.PartialOperationalCertificate => UnsignedBlockHeader
        ): F[Option[BlockHeader]] =
          OptionT(operationalKeyMaker.operationalKeyForSlot(slot, parentSlotId)).semiflatMap { operationalKeyOut =>
            for {
              partialCertificate <- Sync[F].delay(
                UnsignedBlockHeader.PartialOperationalCertificate(
                  operationalKeyOut.parentVK,
                  operationalKeyOut.parentSignature,
                  operationalKeyOut.childVK
                )
              )
              unsignedBlock = unsignedBlockBuilder(partialCertificate)
              messageToSign = unsignedBlock.signableBytes
              signature <- ed25519Resource.use(_.sign(operationalKeyOut.childSK, messageToSign).pure[F])
              operationalCertificate = OperationalCertificate(
                operationalKeyOut.parentVK,
                operationalKeyOut.parentSignature,
                partialCertificate.childVK,
                signature
              )
              header = BlockHeader(
                unsignedBlock.parentHeaderId,
                unsignedBlock.parentSlot,
                unsignedBlock.txRoot,
                unsignedBlock.bloomFilter,
                unsignedBlock.timestamp,
                unsignedBlock.height,
                unsignedBlock.slot,
                unsignedBlock.eligibilityCertificate,
                operationalCertificate,
                unsignedBlock.metadata,
                unsignedBlock.address
              )
            } yield header
          }.value

        def getHit(relativeStake: Ratio, slot: Slot, slotDiff: Long, eta: Eta): F[Option[VrfHit]] =
          for {
            threshold <- leaderElectionValidation.getThreshold(relativeStake, slotDiff)
            testProof <- vrfCalculator.proofForSlot(slot, eta)
            rho       <- vrfCalculator.rhoForSlot(slot, eta)
            isLeader  <- leaderElectionValidation.isSlotLeaderForThreshold(threshold)(rho)
            vrfHit <- OptionT
              .whenF[F, VrfHit](isLeader)(
                blake2b256Resource
                  .use(
                    _.hash(
                      ByteString
                        .copyFrom(threshold.numerator.toByteArray)
                        .concat(
                          ByteString.copyFrom(threshold.denominator.toByteArray)
                        )
                    ).pure[F]
                  )
                  .map(thresholdEvidence =>
                    VrfHit(
                      EligibilityCertificate(testProof, vkVrf, thresholdEvidence, eta.data),
                      slot,
                      threshold
                    )
                  )
              )
              .value
          } yield vrfHit

      }
    }
}
