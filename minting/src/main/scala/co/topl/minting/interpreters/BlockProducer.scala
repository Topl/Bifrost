package co.topl.minting.interpreters

import cats.data.{Chain, OptionT}
import cats.effect._
import cats.implicits._
import co.topl.algebras.ClockAlgebra
import co.topl.catsakka._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.minting.algebras.{BlockPackerAlgebra, BlockProducerAlgebra, StakingAlgebra}
import co.topl.minting.models.VrfHit
import co.topl.models._
import co.topl.typeclasses.implicits._
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.log4cats.{Logger, SelfAwareStructuredLogger}

import fs2._

import scala.collection.immutable.ListSet

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
    parentHeaders: Stream[F, SlotData],
    staker:        StakingAlgebra[F],
    clock:         ClockAlgebra[F],
    blockPacker:   BlockPackerAlgebra[F]
  ): F[BlockProducerAlgebra[F]] =
    staker.address.map(new Impl[F](_, parentHeaders, staker, clock, blockPacker))

  private class Impl[F[_]: Async](
    stakerAddress: StakingAddresses.Operator,
    parentHeaders: Stream[F, SlotData],
    staker:        StakingAlgebra[F],
    clock:         ClockAlgebra[F],
    blockPacker:   BlockPackerAlgebra[F]
  ) extends BlockProducerAlgebra[F] {

    implicit private val logger: SelfAwareStructuredLogger[F] =
      Slf4jLogger.getLoggerFromName[F]("Bifrost.BlockProducer")

    val blocks: F[Stream[F, Block]] =
      Sync[F].delay(
        parentHeaders
          .through(AbandonerPipe(makeChild))
          .collect { case Some(block) => block }
      )

    /**
     * Construct a new child Block of the given parent
     */
    private def makeChild(parentSlotData: SlotData): F[Option[Block]] =
      Async[F].onCancel(
        for {
          // From the given parent block, when are we next eligible to produce a new block?
          nextHit <- nextEligibility(parentSlotData.slotId)
          _ <- Logger[F].debug(
            show"Packing block for" +
            show" parentId=${parentSlotData.slotId.blockId}" +
            show" parentSlot=${parentSlotData.slotId.slot}" +
            show" eligibilitySlot=${nextHit.slot}"
          )
          // Assemble the transactions to be placed in our new block
          body      <- packBlock(parentSlotData.slotId.blockId, parentSlotData.height + 1, nextHit.slot)
          timestamp <- clock.slotToTimestamps(nextHit.slot).map(_.last)
          blockMaker = prepareUnsignedBlock(parentSlotData, body, timestamp, nextHit)
          // Despite being eligible, there may not have a corresponding linear KES key if, for example, the node
          // restarts in the middle of an operational period.  The node must wait until the next operational period
          // to have a set of corresponding linear keys to work with
          maybeBlock <- staker.certifyBlock(parentSlotData.slotId, nextHit.slot, blockMaker)
          _ <- OptionT
            .fromOption[F](maybeBlock)
            .semiflatTap(block => Logger[F].info(show"Minted header=${block.header} body=${block.body}"))
            .value
        } yield maybeBlock,
        Async[F].defer(Logger[F].info(show"Abandoned block attempt on parentId=${parentSlotData.slotId.blockId}"))
      )

    /**
     * Determine the staker's next eligibility based on the given parent
     */
    private def nextEligibility(parentSlotId: SlotId): F[VrfHit] =
      clock.globalSlot
        .map(_.max(parentSlotId.slot + 1))
        .flatMap(
          _.tailRecM(testSlot => OptionT(staker.elect(parentSlotId, testSlot)).toRight(testSlot + 1).value)
        )

    /**
     * Launch the block packer function, then delay the clock, then stop the block packer function and
     * capture the result.  The goal is to grant as much time as possible to the block packer function to produce
     * the best possible block.
     *
     * @param untilSlot The slot at which the block packer function should be halted and a value extracted
     */
    private def packBlock(parentId: TypedIdentifier, height: Long, untilSlot: Slot): F[BlockBody.Full] =
      blockPacker
        .improvePackedBlock(parentId, height, untilSlot)
        .flatMap(Iterative.run(Chain.empty[Transaction].pure[F]))
        .productL(clock.delayedUntilSlot(untilSlot))
        .flatMap(_.apply())

    /**
     * After the block body has been constructed, prepare a Block Header for signing
     */
    private def prepareUnsignedBlock(
      parentSlotData: SlotData,
      body:           BlockBody.Full,
      timestamp:      Timestamp,
      nextHit:        VrfHit
    ): BlockHeader.Unsigned.PartialOperationalCertificate => Block.Unsigned =
      (partialOperationalCertificate: BlockHeader.Unsigned.PartialOperationalCertificate) =>
        Block
          .Unsigned(
            BlockHeader.Unsigned(
              parentHeaderId = parentSlotData.slotId.blockId,
              parentSlot = parentSlotData.slotId.slot,
              txRoot = body.merkleTreeRootHash,
              bloomFilter = body.bloomFilter,
              timestamp = timestamp,
              height = parentSlotData.height + 1,
              slot = nextHit.slot,
              eligibilityCertificate = nextHit.cert,
              partialOperationalCertificate = partialOperationalCertificate,
              metadata = None,
              address = stakerAddress
            ),
            ListSet.empty[TypedIdentifier] ++ body.map(_.id.asTypedBytes).toList
          )
  }

}
