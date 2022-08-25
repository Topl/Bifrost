package co.topl.minting

import akka.NotUsed
import akka.actor.typed.ActorSystem
import akka.stream.{Materializer, OverflowStrategy}
import akka.stream.scaladsl.Source
import cats.data.{Chain, OptionT}
import cats.implicits._
import cats.effect._
import co.topl.algebras.ClockAlgebra
import co.topl.catsakka._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.minting.algebras.LeaderElectionMintingAlgebra.VrfHit
import co.topl.typeclasses.implicits._
import co.topl.minting.algebras.{BlockPackerAlgebra, BlockProducerAlgebra, StakingAlgebra}
import co.topl.models._

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
    parentHeaders:         SourceMatNotUsed[SlotData],
    staker:                StakingAlgebra[F],
    clock:                 ClockAlgebra[F],
    blockPacker:           BlockPackerAlgebra[F]
  )(implicit system: ActorSystem[_]): F[BlockProducerAlgebra[F]] =
    staker.address.map(stakerAddress =>
      new BlockProducerAlgebra[F] {

        val blocks: F[Source[BlockV2, NotUsed]] =
          Sync[F].delay(
            parentHeaders
              .buffer(1, OverflowStrategy.dropHead)
              .via(AbandonerFlow(makeChild(stakerAddress)))
          )

        /**
         * Construct a new child Block of the given parent
         */
        private def makeChild(stakerAddress: StakingAddresses.Operator)(parentSlotData: SlotData) =
          for {
            nextHit   <- nextEligibility(parentSlotData)
            body      <- packBlock(parentSlotData.slotId.blockId, nextHit.slot)
            timestamp <- clock.currentTimestamp
            blockMaker = prepareUnsignedBlock(stakerAddress)(parentSlotData, body, timestamp, nextHit)
            block <- OptionT(staker.certifyBlock(parentSlotData.slotId, nextHit.slot, blockMaker))
              .getOrRaise(new IllegalStateException("Unable to certify block"))
          } yield block

        /**
         * Determine the staker's next eligibility based on the given parent
         */
        private def nextEligibility(parentSlotData: SlotData): F[VrfHit] =
          (parentSlotData.slotId.slot + 1)
            .tailRecM(testSlot => OptionT(staker.elect(parentSlotData, testSlot)).toRight(testSlot + 1).value)

        /**
         * Launch the block packer function, then delay the clock, then stop the block packer function and
         * capture the result.
         */
        private def packBlock(parentId: TypedIdentifier, untilSlot: Slot): F[BlockBodyV2.Full] =
          blockPacker
            .improvePackedBlock(parentId)
            .flatMap(Iterative.runActor(Chain.empty[Transaction].pure[F]))
            .productL(clock.delayedUntilSlot(untilSlot))
            .flatMap(_.apply())

        private def prepareUnsignedBlock(
          stakerAddress: StakingAddresses.Operator
        )(
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
