package co.topl.consensus

import cats._
import cats.data._
import cats.implicits._
import co.topl.algebras.UnsafeResource
import co.topl.crypto.hash.Blake2b512
import co.topl.crypto.signing.Ed25519VRF
import co.topl.models._
import co.topl.typeclasses._
import co.topl.typeclasses.implicits._
import org.typelevel.log4cats.Logger

import scala.annotation.tailrec

/**
 * Provides functionality for deterministically choosing the "better" of two tines (segments of blocks)
 */
object ChainSelection {

  /**
   * The normal ordering to use between tines with a recent common ancestor
   */
  private def standardOrder(implicit blake2b512: Blake2b512): Order[NonEmptyChain[SlotData]] = {
    val lengthOrder = Order.by[NonEmptyChain[SlotData], Long](_.length)
    val slotOrder = Order.by[NonEmptyChain[SlotData], Slot](-_.last.slotId.slot)
    val rhoTestHashOrder =
      Order.reverse(
        Order.by[NonEmptyChain[SlotData], BigInt](h =>
          BigInt(Ed25519VRF.rhoToRhoTestHash(h.last.rho).sizedBytes.data.toArray)
        )
      )

    lengthOrder
      .tiebreakWith(slotOrder)
      .tiebreakWith(rhoTestHashOrder)
  }

  /**
   * Between two tines, determines which one is "better"
   * @param kLookback The number of blocks in the backward-moving window of blocks for longest-chain rule
   * @param sWindow The number of slots of the forward-moving window of blocks for chain-density rule
   */
  def orderT[F[_]: Monad: Logger](
    storage:            SlotDataCache[F],
    blake2b512Resource: UnsafeResource[F, Blake2b512],
    kLookback:          Long,
    sWindow:            Long
  ): OrderT[F, SlotData] = new ChainSelectionOrderT[F](storage, blake2b512Resource, kLookback, sWindow)

  /**
   * Implementation of OrderT which provides F[_]-context-based ordering to SlotData (block headers)
   */
  private class ChainSelectionOrderT[F[_]: Monad: Logger](
    storage:            SlotDataCache[F],
    blake2b512Resource: UnsafeResource[F, Blake2b512],
    kLookback:          Long,
    sWindow:            Long
  ) extends OrderT[F, SlotData] {

    override def compare(x: SlotData, y: SlotData): F[Int] =
      if (x === y) 0.pure[F]
      else
        TineComparisonTraversal
          .build(x, y)
          .flatTap(logTraversal)
          .flatMap(_.comparisonResult)

    /**
     * Print out the traversal result, but only if an actual chain selection takes place (and not just a simple append)
     */
    private def logTraversal(traversal: TineComparisonTraversal): F[Unit] =
      traversal match {
        case LongestChainTraversal(xSegment, ySegment) if xSegment.length > 1 && ySegment.length > 1 =>
          Logger[F].info(
            "Performing standard chain selection" +
            show" tineX=[${xSegment.length}](${xSegment.head.slotId}..${xSegment.last.slotId})" +
            show" tineY=[${ySegment.length}](${ySegment.head.slotId}..${ySegment.last.slotId})"
          )
        case DensityChainTraversal(xSegment, ySegment) =>
          Logger[F].info(
            "Performing density chain selection" +
            show" tineX=[${xSegment.length}](${xSegment.head.slotId}..${xSegment.last.slotId})" +
            show" tineY=[${ySegment.length}](${ySegment.head.slotId}..${ySegment.last.slotId})"
          )
        case _ =>
          Applicative[F].unit
      }

    /**
     * Represents the traversal of two tines until the tines converge
     * on a common ancestor block.
     */
    sealed abstract private class TineComparisonTraversal {
      def next: F[TineComparisonTraversal]
      def sharesCommonAncestor: Boolean
      def comparisonResult: F[Int]

      /**
       * Compares the heads of each of the given chains.  If the heights are equal, traverses both chains up a block.
       * If the xSegment's lowest height is greater than the ySegment's lowest height, only the xSegment is traversed up
       * and the ySegment is returned as-is.  Otherwise, the ySegment is traversed up and the xSegment is returned as-is.
       * @param xSegment a segment of chain data
       * @param ySegment a segment of chain data
       * @tparam C Some non-empty, prependable collection type (i.e. NonEmptyChain or NonEmptyVector)
       * @return a tuple containing (newXSegment, newYSegment)
       */
      protected def prependSegments[C[_]: Prepend: NonEmpty](
        xSegment: C[SlotData],
        ySegment: C[SlotData]
      ): F[(C[SlotData], C[SlotData])] = {

        def prependSegment(segment: C[SlotData]): F[C[SlotData]] =
          for {
            parent <- storage.get(NonEmpty[C].head(segment).parentSlotId.blockId)
            prepended = Prepend[C].prepend(parent, segment)
          } yield prepended

        val xLowestHeight = NonEmpty[C].head(xSegment).height
        val yLowestHeight = NonEmpty[C].head(ySegment).height

        xLowestHeight.comparison(yLowestHeight) match {
          case Comparison.EqualTo     => (prependSegment(xSegment), prependSegment(ySegment)).tupled
          case Comparison.GreaterThan => (prependSegment(xSegment), ySegment.pure[F]).tupled
          case Comparison.LessThan    => (xSegment.pure[F], prependSegment(ySegment)).tupled
        }
      }
    }

    private object TineComparisonTraversal {

      /**
       * Assembles an entire traversal (to some common ancestor) given two tine heads.  This operation starts
       * with a LongestChainTraversal but will switch to a DensityTraversal if the traversal digs deep enough
       */
      def build(x: SlotData, y: SlotData): F[TineComparisonTraversal] =
        (LongestChainTraversal(NonEmptyChain.one(x), NonEmptyChain.one(y)): TineComparisonTraversal)
          .iterateWhileM(_.next)(!_.sharesCommonAncestor)
    }

    /**
     * A tine traversal strategy which aggregates kLookback-number of blocks
     * in a left-growing (right-fixed) window.  Once more than kLookback blocks have been aggregated,
     * the strategy switches to [[DensityChainTraversal]]
     */
    private case class LongestChainTraversal(
      xSegment: NonEmptyChain[SlotData],
      ySegment: NonEmptyChain[SlotData]
    ) extends TineComparisonTraversal {

      def next: F[TineComparisonTraversal] =
        // Prepend/accumulate the next parent header for the xSegment and the ySegment
        for {
          (newXSegment, newYSegment) <- prependSegments(xSegment, ySegment)
          // Once we've traversed back K number of blocks, switch to the chain density rule
          switchToChainDensity = (newXSegment.length > kLookback) || (newYSegment.length > kLookback)
          nextTraversal =
            if (switchToChainDensity)
              DensityChainTraversal(newXSegment.toNonEmptyVector, newYSegment.toNonEmptyVector).slicedWithinSWindow
            else
              LongestChainTraversal(newXSegment, newYSegment)
        } yield nextTraversal

      def sharesCommonAncestor: Boolean =
        xSegment.head === ySegment.head

      def comparisonResult: F[Int] =
        blake2b512Resource.use(implicit blake2b512 => standardOrder.compare(xSegment, ySegment).pure[F])
    }

    /**
     * A tine traversal strategy which aggregates a slot-based window of blocks.  The window is right-growing (left-fixed).
     * This aggregation process involves prepending a segment of chain and subsequently dropping the right-most elements
     * which do not fit within the `sWindow` based on the slot of the left-most element.
     */
    private case class DensityChainTraversal(xSegment: NonEmptyVector[SlotData], ySegment: NonEmptyVector[SlotData])
        extends TineComparisonTraversal {

      def next: F[TineComparisonTraversal] =
        for {
          (newXSegment, newYSegment) <- prependSegments(xSegment, ySegment)
        } yield DensityChainTraversal(newXSegment, newYSegment).slicedWithinSWindow

      def sharesCommonAncestor: Boolean =
        xSegment.head === ySegment.head

      def comparisonResult: F[Int] =
        xSegment.length.compare(ySegment.length).pure[F]

      /**
       * In cases where a common-ancestor search traces back more than `kLookback` blocks, we only need to aggregate
       * blocks within the forward-looking sWindow.  The result is used when calculating chain density.
       * TODO: It works, but could be improved
       * @example
       * sWindow = 200
       * A : [10,a]..[577,z]
       * A1: [10,a]..[210,y]
       * A2: [10,a]..[207,x]
       */
      def slicedWithinSWindow: DensityChainTraversal = {
        @tailrec
        def inner(segment: NonEmptyVector[SlotData]): NonEmptyVector[SlotData] =
          if (segment.length === 1) segment
          else if (segment.last.slotId.slot - segment.head.slotId.slot > sWindow)
            NonEmptyVector.fromVector(segment.init) match {
              case Some(value) => inner(value)
              case _           => segment // This case shouldn't actually happen because of the first `if` condition
            }
          else segment
        copy(inner(xSegment), inner(ySegment))
      }
    }
  }
}
