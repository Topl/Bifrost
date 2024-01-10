package co.topl.consensus.interpreters

import cats._
import cats.data._
import cats.effect._
import cats.implicits._
import co.topl.consensus.algebras.ChainSelectionAlgebra
import co.topl.consensus.models.{ChainSelectionOutcome, SlotData}
import co.topl.consensus.rhoToRhoTestHash
import co.topl.crypto.hash.Blake2b512
import co.topl.models._
import co.topl.models.utility._
import co.topl.typeclasses.implicits._
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

/**
 * Provides functionality for deterministically choosing the "better" of two tines (segments of blocks)
 */
object ChainSelection {

  private val heightOrder = Order.by[SlotData, Long](_.height)
  private val slotOrder = Order.by[SlotData, Slot](-_.slotId.slot)

  private def rhoTestHashOrderSlotData(implicit blake2b512: Blake2b512): Order[SlotData] =
    Order.reverse(
      Order.by[SlotData, BigInt](h => BigInt(rhoToRhoTestHash(h.rho).toArray))
    )

  /**
   * The normal ordering to use between tines with a recent common ancestor
   */
  private def standardOrder(implicit blake2b512: Blake2b512): Order[SlotData] =
    heightOrder
      .tiebreakWith(slotOrder)
      .tiebreakWith(rhoTestHashOrderSlotData)

  implicit private class OrderSupport[T](order: Order[T]) {

    def tiebreakWith(other: Order[T]): Order[T] =
      Order.whenEqual(order, other)
  }

  /**
   * Between two tines, determines which one is "better"
   *
   * @param kLookback The number of blocks in the backward-moving window of blocks for longest-chain rule
   * @param sWindow   The number of slots of the forward-moving window of blocks for chain-density rule
   */
  def make[F[_]: Sync](
    blake2b512Resource: Resource[F, Blake2b512],
    kLookback:          Long,
    sWindow:            Long
  ): ChainSelectionAlgebra[F] =
    new ChainSelectionImpl[F](blake2b512Resource, kLookback, sWindow)

  /**
   * Implementation of OrderT which provides F[_]-context-based ordering to SlotData (block headers)
   */
  private class ChainSelectionImpl[F[_]: Sync](
    blake2b512Resource: Resource[F, Blake2b512],
    kLookback:          Long,
    sWindow:            Long
  ) extends ChainSelectionAlgebra[F] {

    implicit private val logger: Logger[F] =
      Slf4jLogger.getLoggerFromName("Bifrost.ChainSelection")

    def compare(
      xHead:          SlotData,
      yHead:          SlotData,
      commonAncestor: SlotData,
      fetchXAtHeight: Long => F[Option[SlotData]],
      fetchYAtHeight: Epoch => F[Option[SlotData]]
    ): F[ChainSelectionOutcome] = (
      if (yHead.slotId.blockId == commonAncestor.slotId.blockId)
        Sync[F].pure[ChainSelectionOutcome](ChainSelectionOutcome.XStandard)
      else if (xHead.slotId.blockId == commonAncestor.slotId.blockId)
        Sync[F].pure[ChainSelectionOutcome](ChainSelectionOutcome.YStandard)
      else if (xHead.height - commonAncestor.height <= kLookback && yHead.height - commonAncestor.height <= kLookback)
        standardOrderOutcome(xHead, yHead)
      else {
        Logger[F].info("Starting density chain selection process") >>
        densityBoundaryBlock(commonAncestor, fetchXAtHeight).flatMap(xBoundary =>
          OptionT(fetchYAtHeight(xBoundary.height))
            .filterNot(_.slotId.slot - commonAncestor.slotId.slot > sWindow)
            .foldF[ChainSelectionOutcome](ChainSelectionOutcome.XDensity.pure[F].widen)(yAtX =>
              OptionT(fetchYAtHeight(xBoundary.height + 1))
                .filterNot(_.slotId.slot - commonAncestor.slotId.slot > sWindow)
                .as(ChainSelectionOutcome.YDensity)
                .getOrElseF(
                  // Tie Breaker
                  standardOrderOutcome(xBoundary, yAtX)
                    .map(outcome => if (outcome.isX) ChainSelectionOutcome.XDensity else ChainSelectionOutcome.YDensity)
                )
            )
        )
      }
    )
      .flatTap(logOutcome(xHead, yHead))

    private def standardOrderOutcome(x: SlotData, y: SlotData): F[ChainSelectionOutcome] =
      blake2b512Resource.use(implicit b => Sync[F].delay(standardOrder.comparison(x, y))).map {
        case Comparison.LessThan => ChainSelectionOutcome.YStandard
        case _                   => ChainSelectionOutcome.XStandard
      }

    /**
     * Starting from the given common ancestor, traverses forward along the chain, keeping all blocks that fall within
     * the protocol's `sWindow` setting.
     *
     * TODO: Optimization: Use (fEffective * sWindow) + commonAncestor.height to guide the search process
     *
     * @param commonAncestor The starting point for the search
     * @param fetchHeader A lookup function to find a block-by-height
     * @return The "best" SlotData that comes after the common ancestor but within the protocol's sWindow
     */
    private def densityBoundaryBlock(commonAncestor: SlotData, fetchHeader: Long => F[Option[SlotData]]): F[SlotData] =
      commonAncestor.tailRecM(currentBoundary =>
        OptionT(fetchHeader(currentBoundary.height + 1))
          .filterNot(_.slotId.slot - commonAncestor.slotId.slot > sWindow)
          .toLeft(currentBoundary)
          .value
      )

    private def logOutcome(xHead: SlotData, yHead: SlotData)(outcome: ChainSelectionOutcome): F[Unit] =
      outcome match {
        case ChainSelectionOutcome.XStandard =>
          Logger[F].info(
            show"X(id=${xHead.slotId.blockId}) is better than Y(id=${yHead.slotId.blockId}) using algorithm=standard"
          )
        case ChainSelectionOutcome.YStandard =>
          Logger[F].info(
            show"Y(id=${yHead.slotId.blockId}) is better than X(id=${xHead.slotId.blockId}) using algorithm=standard"
          )
        case ChainSelectionOutcome.XDensity =>
          Logger[F].info(
            show"X(id=${xHead.slotId.blockId}) is better than Y(id=${yHead.slotId.blockId}) using algorithm=density"
          )
        case ChainSelectionOutcome.YDensity =>
          Logger[F].info(
            show"Y(id=${yHead.slotId.blockId}) is better than X(id=${xHead.slotId.blockId}) using algorithm=density"
          )
      }
  }
}
