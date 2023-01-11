package co.topl.minting.interpreters

import cats.Applicative
import cats.data.OptionT
import cats.effect.Sync
import cats.implicits._
import co.topl.algebras.{ClockAlgebra, UnsafeResource}
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.consensus.algebras.{ConsensusValidationStateAlgebra, EtaCalculationAlgebra}
import co.topl.crypto.signing.Ed25519
import co.topl.minting.algebras._
import co.topl.minting.models.VrfHit
import co.topl.models._
import co.topl.models.utility.HasLength.instances.bytesLength
import co.topl.models.utility.Sized
import co.topl.typeclasses.implicits._
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.log4cats.{Logger, SelfAwareStructuredLogger}

object Staking {

  def make[F[_]: Sync](
    a:                   StakingAddresses.Operator,
    operationalKeyMaker: OperationalKeyMakerAlgebra[F],
    consensusState:      ConsensusValidationStateAlgebra[F],
    etaCalculation:      EtaCalculationAlgebra[F],
    ed25519Resource:     UnsafeResource[F, Ed25519],
    vrfCalculator:       VrfCalculatorAlgebra[F],
    clock:               ClockAlgebra[F]
  ): StakingAlgebra[F] = new StakingAlgebra[F] {
    implicit private val logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromClass[F](Staking.getClass)
    val address: F[StakingAddresses.Operator] = a.pure[F]

    def elect(parentSlotId: SlotId, slot: Slot): F[Option[VrfHit]] =
      for {
        slotsPerEpoch <- clock.slotsPerEpoch
        eta           <- etaCalculation.etaToBe(parentSlotId, slot)
        _ <- Applicative[F].whenA(slot % slotsPerEpoch === 0L)(
          vrfCalculator.precomputeForEpoch(slot / slotsPerEpoch, eta)
        )
        maybeHit <- OptionT(consensusState.operatorRelativeStake(parentSlotId.blockId, slot)(a))
          .flatMapF(relativeStake => vrfCalculator.getHit(relativeStake, slot, slot - parentSlotId.slot, eta))
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
      OptionT(operationalKeyMaker.operationalKeyForSlot(slot, parentSlotId)).semiflatMap { operationalKeyOut =>
        for {
          partialCertificate <- Sync[F].delay(
            BlockHeader.Unsigned.PartialOperationalCertificate(
              operationalKeyOut.parentVK,
              operationalKeyOut.parentSignature,
              operationalKeyOut.childVK
            )
          )
          unsignedBlock = unsignedBlockBuilder(partialCertificate)
          messageToSign = unsignedBlock.unsignedHeader.signableBytes
          signature <- ed25519Resource.use(_.sign(operationalKeyOut.childSK.bytes.data, messageToSign).pure[F])
          operationalCertificate = OperationalCertificate(
            operationalKeyOut.parentVK,
            operationalKeyOut.parentSignature,
            partialCertificate.childVK,
            Proofs.Knowledge.Ed25519(Sized.strictUnsafe(signature))
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
          block = Block(header, unsignedBlock.body)
        } yield block
      }.value
  }

}
