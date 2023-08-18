package co.topl.minting.interpreters

import cats.data.OptionT
import cats.effect.Resource
import cats.effect.Sync
import cats.implicits._
import co.topl.algebras.UnsafeResource
import co.topl.brambl.models.LockAddress
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

  def make[F[_]: Sync](
    a:                        StakingAddress,
    rewardAddress:            LockAddress,
    vkVrf:                    ByteString,
    operationalKeyMaker:      OperationalKeyMakerAlgebra[F],
    consensusState:           ConsensusValidationStateAlgebra[F],
    etaCalculation:           EtaCalculationAlgebra[F],
    ed25519Resource:          UnsafeResource[F, Ed25519],
    blake2b256Resource:       UnsafeResource[F, Blake2b256],
    vrfCalculator:            VrfCalculatorAlgebra[F],
    leaderElectionValidation: LeaderElectionValidationAlgebra[F],
    protocolVersion:          ProtocolVersion
  ): Resource[F, StakingAlgebra[F]] =
    Resource.pure {
      val _rewardAddress = rewardAddress
      new StakingAlgebra[F] {
        implicit private val logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromClass[F](Staking.getClass)
        val address: F[StakingAddress] = a.pure[F]
        val rewardAddress: F[LockAddress] = _rewardAddress.pure[F]

        def elect(parentSlotId: SlotId, slot: Slot): F[Option[VrfHit]] =
          (
            for {
              eta           <- OptionT.liftF(etaCalculation.etaToBe(parentSlotId, slot))
              relativeStake <- OptionT(consensusState.operatorRelativeStake(parentSlotId.blockId, slot)(a))
              threshold <- OptionT.liftF(leaderElectionValidation.getThreshold(relativeStake, slot - parentSlotId.slot))
              testProof <- OptionT.liftF(vrfCalculator.proofForSlot(slot, eta))
              rho       <- OptionT.liftF(vrfCalculator.rhoForSlot(slot, eta))
              isLeader  <- OptionT.liftF(leaderElectionValidation.isSlotLeaderForThreshold(threshold)(rho))
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
          ).value.flatTap(maybeHit =>
            Logger[F].debug(
              show"Eligibility at" +
              show" slot=$slot" +
              show" parentId=${parentSlotId.blockId}" +
              show" parentSlot=${parentSlotId.slot}" +
              show" eligible=${maybeHit.nonEmpty}"
            )
          )

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
