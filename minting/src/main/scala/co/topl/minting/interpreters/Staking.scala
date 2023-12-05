package co.topl.minting.interpreters

import cats.data.OptionT
import cats.effect._
import cats.implicits._
import co.topl.brambl.models.LockAddress
import co.topl.catsutils._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.consensus.algebras._
import co.topl.consensus.models._
import co.topl.consensus.thresholdEvidence
import co.topl.crypto.hash.Blake2b256
import co.topl.crypto.signing.Ed25519
import co.topl.minting.algebras._
import co.topl.minting.models.VrfHit
import co.topl.models._
import co.topl.typeclasses.implicits._
import com.google.protobuf.ByteString
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

object Staking {

  def make[F[_]: Async](
    a:                        StakingAddress,
    rewardAddress:            LockAddress,
    vkVrf:                    ByteString,
    operationalKeyMaker:      OperationalKeyMakerAlgebra[F],
    consensusState:           ConsensusValidationStateAlgebra[F],
    etaCalculation:           EtaCalculationAlgebra[F],
    ed25519Resource:          Resource[F, Ed25519],
    blake2b256Resource:       Resource[F, Blake2b256],
    vrfCalculator:            VrfCalculatorAlgebra[F],
    leaderElectionValidation: LeaderElectionValidationAlgebra[F],
    protocolVersion:          ProtocolVersion
  ): Resource[F, StakingAlgebra[F]] =
    Resource
      .pure {
        val _rewardAddress = rewardAddress
        new StakingAlgebra[F] {
          implicit private val logger: SelfAwareStructuredLogger[F] =
            Slf4jLogger.getLoggerFromClass[F](Staking.getClass)
          val address: F[StakingAddress] = a.pure[F]
          val rewardAddress: F[LockAddress] = _rewardAddress.pure[F]

          def elect(parentSlotId: SlotId, slot: Slot): F[Option[VrfHit]] = (
            for {
              eta <- OptionT.liftF(etaCalculation.etaToBe(parentSlotId, slot))
              relativeStake <- OptionT(
                consensusState
                  .operatorRelativeStake(parentSlotId.blockId, slot)(a)
                  .warnIfSlow("Local Operator Relative Stake")
              ).flatTapNone(
                Logger[F].debug(s"No stake at slot=$slot")
              )
              threshold <- OptionT.liftF(
                leaderElectionValidation
                  .getThreshold(relativeStake, slot - parentSlotId.slot)
                  .warnIfSlow("Local threshold")
              )
              testProof <- OptionT.liftF(vrfCalculator.proofForSlot(slot, eta))
              rho       <- OptionT.liftF(vrfCalculator.rhoForSlot(slot, eta))
              isLeader <- OptionT.liftF(
                leaderElectionValidation
                  .isSlotLeaderForThreshold(threshold)(rho)
                  .warnIfSlow("Local Operator Is Slot Leader")
              )
              _ <- OptionT.liftF(
                Logger[F].debug(
                  show"Eligibility at" +
                  show" slot=$slot" +
                  show" parentId=${parentSlotId.blockId}" +
                  show" parentSlot=${parentSlotId.slot}" +
                  show" eligible=$isLeader" +
                  show" relativeStake=$relativeStake"
                )
              )
              vrfHit <- OptionT
                .whenF[F, VrfHit](isLeader)(
                  blake2b256Resource
                    .use(implicit b => Sync[F].delay(thresholdEvidence(threshold)))
                    .map(evidence =>
                      VrfHit(
                        EligibilityCertificate(testProof, vkVrf, evidence, eta.data),
                        slot,
                        threshold
                      )
                    )
                )
            } yield vrfHit
          ).value

          def certifyBlock(
            parentSlotId:         SlotId,
            slot:                 Slot,
            unsignedBlockBuilder: UnsignedBlockHeader.PartialOperationalCertificate => UnsignedBlockHeader,
            eta:                  Eta
          ): F[Option[BlockHeader]] =
            OptionT(operationalKeyMaker.operationalKeyForSlot(slot, parentSlotId, eta)).semiflatMap {
              operationalKeyOut =>
                for {
                  partialCertificate <- Sync[F].delay(
                    UnsignedBlockHeader.PartialOperationalCertificate(
                      operationalKeyOut.parentVK,
                      operationalKeyOut.parentSignature,
                      operationalKeyOut.childVK
                    )
                  )
                  unsignedBlock = unsignedBlockBuilder(partialCertificate)
                  messageToSign = unsignedBlock.signableBytes.toByteArray
                  signature <- ed25519Resource.use(ed25519 =>
                    Sync[F].delay(
                      ed25519.sign(
                        Ed25519.SecretKey(operationalKeyOut.childSK.toByteArray),
                        messageToSign
                      )
                    )
                  )
                  operationalCertificate = OperationalCertificate(
                    operationalKeyOut.parentVK,
                    operationalKeyOut.parentSignature,
                    partialCertificate.childVK,
                    ByteString.copyFrom(signature)
                  )
                  header = BlockHeader(
                    headerId = None,
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
                    unsignedBlock.address,
                    protocolVersion
                  )
                } yield header
            }.value
        }
      }
}
