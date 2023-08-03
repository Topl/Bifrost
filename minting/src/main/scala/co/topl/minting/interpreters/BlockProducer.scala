package co.topl.minting.interpreters

import cats.Applicative
import cats.data.OptionT
import cats.effect._
import cats.implicits._
import co.topl.algebras.ClockAlgebra
import co.topl.brambl.models._
import co.topl.brambl.models.box._
import co.topl.brambl.models.transaction._
import co.topl.brambl.syntax._
import co.topl.catsutils._
import co.topl.consensus.models.{BlockId, SlotData, SlotId, StakingAddress}
import co.topl.ledger.algebras.TransactionRewardCalculatorAlgebra
import co.topl.minting.algebras.{BlockPackerAlgebra, BlockProducerAlgebra, StakingAlgebra}
import co.topl.minting.models.VrfHit
import co.topl.models._
import co.topl.node.models.{BlockBody, FullBlock, FullBlockBody}
import co.topl.numerics.implicits._
import co.topl.typeclasses.implicits._
import com.google.protobuf.ByteString
import fs2._
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.log4cats.{Logger, SelfAwareStructuredLogger}
import quivr.models.SmallData

object BlockProducer {

  /**
   * Creates a BlockProducerAlgebra which emits blocks whenever the staker is eligible.  Eligibility is determined
   * by the parent block header (among other factors), so any time the canonical head changes, the BlockProducer abandons
   * any work it was doing previously and starts building from the new parent header.  Each new parent header results
   * in a new "next" eligibility/slot.  The BlockProducer will give as much time as possible to the Block Packer
   * before instructing the Block Packer to provide its current best result and forming it into a Block.
   * @param parentHeaders a stream of Block Headers (slot data), where each Block Header is the current canonical head
   *                      of the chain
   * @param blockPacker a function which returns a Source that should emit a single element when demanded.  The Source
   *                    should immediately start constructing a result once created, and it should emit its best attempt
   *                    when demanded.
   */
  def make[F[_]: Async](
    parentHeaders:    Stream[F, SlotData],
    staker:           StakingAlgebra[F],
    clock:            ClockAlgebra[F],
    blockPacker:      BlockPackerAlgebra[F],
    rewardCalculator: TransactionRewardCalculatorAlgebra[F],
    rewardAddress:    LockAddress
  ): F[BlockProducerAlgebra[F]] =
    staker.address.map(new Impl[F](_, parentHeaders, staker, clock, blockPacker, rewardCalculator, rewardAddress))

  private class Impl[F[_]: Async](
    stakerAddress:    StakingAddress,
    parentHeaders:    Stream[F, SlotData],
    staker:           StakingAlgebra[F],
    clock:            ClockAlgebra[F],
    blockPacker:      BlockPackerAlgebra[F],
    rewardCalculator: TransactionRewardCalculatorAlgebra[F],
    rewardAddress:    LockAddress
  ) extends BlockProducerAlgebra[F] {

    implicit private val logger: SelfAwareStructuredLogger[F] =
      Slf4jLogger.getLoggerFromName[F]("Bifrost.BlockProducer")

    val blocks: F[Stream[F, FullBlock]] =
      Sync[F].delay(parentHeaders.evalFilter(isRecentParent).through(AbandonerPipe(makeChild)))

    /**
     * Determines if the given SlotData is recent enough to be used as a parent for a new block.
     * @param parentSlotData The parent to attempt to use for a new block
     * @return true if the parent was created within the last epoch, false otherwise
     */
    private def isRecentParent(parentSlotData: SlotData) =
      (clock.globalSlot, clock.slotsPerEpoch)
        .mapN((currentSlot, epochLength) => (currentSlot - parentSlotData.slotId.slot) < epochLength)
        .flatTap(isRecent =>
          if (!isRecent)
            Logger[F].warn(
              show"Skipping block production on parent=${parentSlotData.slotId.blockId.show}" +
              show" because more than one epoch has elapsed since it was created." +
              show" Awaiting new block from network peer."
            )
          else Applicative[F].unit
        )

    /**
     * Construct a new child Block of the given parent
     */
    private def makeChild(parentSlotData: SlotData): F[FullBlock] =
      Async[F].onCancel(
        clock.globalSlot >>= attemptUntilCertified(parentSlotData),
        Async[F].defer(Logger[F].info(show"Abandoned block attempt on parentId=${parentSlotData.slotId.blockId}"))
      )

    /**
     * Attempts to produce a new block.  If the staker is eligible but no operational key is available, the attempt
     * will be retried starting in the next operational period.
     */
    private def attemptUntilCertified(parentSlotData: SlotData)(fromSlot: Slot): F[FullBlock] =
      for {
        nextHit <- nextEligibility(parentSlotData.slotId)(fromSlot)
        _ <- Logger[F].debug(
          show"Packing block for" +
          show" parentId=${parentSlotData.slotId.blockId}" +
          show" parentSlot=${parentSlotData.slotId.slot}" +
          show" eligibilitySlot=${nextHit.slot}"
        )
        // Assemble the transactions to be placed in our new block
        fullBody  <- packBlock(parentSlotData.slotId.blockId, parentSlotData.height + 1, nextHit.slot)
        timestamp <- clock.slotToTimestamps(nextHit.slot).map(_.last)
        blockMaker = prepareUnsignedBlock(parentSlotData, fullBody, timestamp, nextHit)
        maybeHeader <- staker.certifyBlock(parentSlotData.slotId, nextHit.slot, blockMaker)
        result <- OptionT
          .fromOption[F](maybeHeader)
          .map(FullBlock(_, fullBody))
          .semiflatTap(block =>
            Sync[F]
              .delay(BlockBody(block.fullBody.transactions.map(_.id), block.fullBody.rewardTransaction.map(_.id)))
              .flatMap(body => Logger[F].info(show"Minted header=${block.header} body=$body"))
          )
          // Despite being eligible, there may not be a corresponding linear KES key if the node restarted in the middle
          // of an operational period.  The node must wait until the next operational period
          // to have a set of corresponding linear keys use.
          .getOrElseF(
            for {
              operationalPeriodLength <- clock.slotsPerOperationalPeriod
              nextOperationalPeriodSlot <- Sync[F]
                .delay((nextHit.slot / operationalPeriodLength + 1) * operationalPeriodLength)
              _ <- Logger[F]
                .warn(
                  show"Operational key unavailable.  Skipping eligibility at slot=${nextHit.slot}" +
                  show" plus any remaining eligibilities until next operational period at slot=$nextOperationalPeriodSlot"
                )
              res <- attemptUntilCertified(parentSlotData)(nextOperationalPeriodSlot)
            } yield res
          )
      } yield result

    /**
     * Determine the staker's next eligibility based on the given parent
     */
    private def nextEligibility(parentSlotId: SlotId)(fromSlot: Slot): F[VrfHit] =
      (fromSlot
        .max(parentSlotId.slot + 1))
        .tailRecM(testSlot => OptionT(staker.elect(parentSlotId, testSlot)).toRight(testSlot + 1).value)

    /**
     * Launch the block packer function, then delay the clock, then stop the block packer function and
     * capture the result.  The goal is to grant as much time as possible to the block packer function to produce
     * the best possible block.
     *
     * @param untilSlot The slot at which the block packer function should be halted and a value extracted
     */
    private def packBlock(parentId: BlockId, height: Long, untilSlot: Slot): F[FullBlockBody] =
      blockPacker
        .improvePackedBlock(parentId, height, untilSlot)
        .flatMap(Iterative.run(FullBlockBody().pure[F]))
        .productL(clock.delayedUntilSlot(untilSlot))
        .flatMap(_.apply())
        .flatMap(insertReward(parentId, untilSlot, _))

    /**
     * Calculate the total block reward quantity, and insert a Reward Transaction into the given FullBlockBody
     * @param parentBlockId the ID of the parent block.  This ID will be used in a fake IoTransaction reference
     *                      as input to the reward transaction
     * @param slot The slot in which the block will be produced.  The resulting transaction is only valid at this slot
     * @param base The unrewarded block body
     * @return a new block body
     */
    private def insertReward(parentBlockId: BlockId, slot: Slot, base: FullBlockBody): F[FullBlockBody] =
      base.transactions
        .foldMapM(rewardCalculator.rewardOf)
        .map(rewardQuantity =>
          if (rewardQuantity > 0)
            base.withRewardTransaction(
              IoTransaction(datum =
                Datum.IoTransaction(
                  Event.IoTransaction(schedule = Schedule(min = slot, max = slot), metadata = SmallData.defaultInstance)
                )
              )
                .withInputs(
                  List(
                    SpentTransactionOutput(
                      address = TransactionOutputAddress(id = TransactionId(parentBlockId.value)),
                      attestation = Attestation.defaultInstance,
                      value = Value.defaultInstance
                    )
                  )
                )
                .withOutputs(
                  List(
                    UnspentTransactionOutput(rewardAddress, Value(Value.Value.Lvl(Value.LVL(rewardQuantity))))
                  )
                )
            )
          else {
            // To avoid dust accumulation for 0-reward blocks, don't include a reward transaction
            base.clearRewardTransaction
          }
        )

    /**
     * After the block body has been constructed, prepare a Block Header for signing
     */
    private def prepareUnsignedBlock(
      parentSlotData: SlotData,
      body:           FullBlockBody,
      timestamp:      Timestamp,
      nextHit:        VrfHit
    ): UnsignedBlockHeader.PartialOperationalCertificate => UnsignedBlockHeader =
      (partialOperationalCertificate: UnsignedBlockHeader.PartialOperationalCertificate) =>
        UnsignedBlockHeader(
          parentHeaderId = parentSlotData.slotId.blockId,
          parentSlot = parentSlotData.slotId.slot,
          txRoot = body.merkleTreeRootHash.data,
          bloomFilter = body.bloomFilter.data,
          timestamp = timestamp,
          height = parentSlotData.height + 1,
          slot = nextHit.slot,
          eligibilityCertificate = nextHit.cert,
          partialOperationalCertificate = partialOperationalCertificate,
          metadata = ByteString.EMPTY,
          address = stakerAddress
        )
  }

}
