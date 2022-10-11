package co.topl.minting

import akka.NotUsed
import akka.actor.typed.ActorSystem
import akka.stream.OverflowStrategy
import akka.stream.scaladsl.Source
import cats.data.{Chain, OptionT}
import cats.effect._
import cats.implicits._
import co.topl.algebras.ClockAlgebra
import co.topl.catsakka._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.minting.algebras.LeaderElectionMintingAlgebra.VrfHit
import co.topl.minting.algebras.{BlockPackerAlgebra, BlockProducerAlgebra, StakingAlgebra}
import co.topl.models._
import co.topl.typeclasses.implicits._
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.log4cats.{Logger, SelfAwareStructuredLogger}

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
  def make[F[_]: Async: FToFuture: RunnableGraphToF](
    parentHeaders:   SourceMatNotUsed[SlotData],
    staker:          StakingAlgebra[F],
    clock:           ClockAlgebra[F],
    blockPacker:     BlockPackerAlgebra[F]
  )(implicit system: ActorSystem[_]): F[BlockProducerAlgebra[F]] =
    staker.address.map(stakerAddress =>
      new BlockProducerAlgebra[F] {
        implicit val logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromClass[F](BlockProducer.getClass)

        val blocks: F[Source[BlockV2, NotUsed]] =
          Sync[F].delay(
            parentHeaders
              .buffer(1, OverflowStrategy.dropHead)
              .via(AbandonerFlow(makeChild))
              .collect { case Some(block) => block }
          )

        /**
         * Construct a new child Block of the given parent
         */
        private def makeChild(parentSlotData: SlotData): F[Option[BlockV2]] =
          for {
            // From the given parent block, when are we next eligible to produce a new block?
            nextHit <- nextEligibility(parentSlotData.slotId)
            _ <- Logger[F].debug(
              show"Packing block for parentId=${parentSlotData.slotId.blockId} parentSlot=${parentSlotData.slotId.slot} eligibilitySlot=${nextHit.slot}"
            )
            // Assemble the transactions to be placed in our new block
            body      <- packBlock(parentSlotData.slotId.blockId, parentSlotData.height + 1, nextHit.slot)
            timestamp <- clock.currentTimestamp
            blockMaker = prepareUnsignedBlock(parentSlotData, body, timestamp, nextHit)
            // Despite being eligible, there may not have a corresponding linear KES key if, for example, the node
            // restarts in the middle of an operational period.  The node must wait until the next operational period
            // to have a set of corresponding linear keys to work with
            maybeBlock <- staker.certifyBlock(parentSlotData.slotId, nextHit.slot, blockMaker)
          } yield maybeBlock

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
         * @param untilSlot The slot at which the block packer function should be halted and a value extracted
         */
        private def packBlock(parentId: TypedIdentifier, height: Long, untilSlot: Slot): F[BlockBodyV2.Full] =
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
          body:           BlockBodyV2.Full,
          timestamp:      Timestamp,
          nextHit:        VrfHit
        ): BlockHeaderV2.Unsigned.PartialOperationalCertificate => BlockV2.Unsigned =
          (partialOperationalCertificate: BlockHeaderV2.Unsigned.PartialOperationalCertificate) =>
            BlockV2
              .Unsigned(
                BlockHeaderV2.Unsigned(
                  parentHeaderId = parentSlotData.slotId.blockId,
                  parentSlot = parentSlotData.slotId.slot,
                  txRoot = body.merkleTree,
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
    )
}
