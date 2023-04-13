package co.topl.consensus.interpreters

import cats._
import cats.data._
import cats.effect.kernel.Sync
import cats.implicits._
import co.topl.algebras.UnsafeResource
import co.topl.consensus.algebras.ChainSelectionAlgebra
import co.topl.consensus.models.BlockId
import co.topl.crypto.hash.Blake2b512
import co.topl.models._
import co.topl.models.utility._
import co.topl.consensus.models.SlotData
import co.topl.consensus.rhoToRhoTestHash
import co.topl.typeclasses.implicits._
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.annotation.tailrec

/**
 * Provides functionality for deterministically choosing the "better" of two tines (segments of blocks)
 */
object ChainSelection {

  import NonEmpty.instances._
  import Prepend.instances._

  private val lengthOrder = Order.by[SlotData, Long](_.height)
  private val slotOrder = Order.by[SlotData, Slot](-_.slotId.slot)

  private def rhoTestHashOrderSlotData(implicit blake2b512: Blake2b512) =
    Order.reverse(
      Order.by[SlotData, BigInt](h => BigInt(rhoToRhoTestHash(h.rho).toArray))
    )

  /**
   * The normal ordering to use between tines with a recent common ancestor
   */
  private def standardOrder(implicit blake2b512: Blake2b512): Order[SlotData] =
    lengthOrder
      .tiebreakWith(slotOrder)
      .tiebreakWith(rhoTestHashOrderSlotData)

  /**
   * Between two tines, determines which one is "better"
   * @param kLookback The number of blocks in the backward-moving window of blocks for longest-chain rule
   * @param sWindow The number of slots of the forward-moving window of blocks for chain-density rule
   */
  def make[F[_]: Sync](
    fetchSlotData:      BlockId => F[SlotData],
    blake2b512Resource: UnsafeResource[F, Blake2b512],
    kLookback:          Long,
    sWindow:            Long
  ): ChainSelectionAlgebra[F, SlotData] =
    new ChainSelectionImpl[F](fetchSlotData, blake2b512Resource, kLookback, sWindow)

  /**
   * Implementation of OrderT which provides F[_]-context-based ordering to SlotData (block headers)
   */
  private class ChainSelectionImpl[F[_]: Sync](
    fetchSlotData:      BlockId => F[SlotData],
    blake2b512Resource: UnsafeResource[F, Blake2b512],
    kLookback:          Long,
    sWindow:            Long
  ) extends ChainSelectionAlgebra[F, SlotData] {

    implicit private val logger: Logger[F] =
      Slf4jLogger.getLoggerFromName("Bifrost.ChainSelection")

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
            parent <- fetchSlotData(NonEmpty[C].head(segment).parentSlotId.blockId)
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
        blake2b512Resource.use(implicit blake2b512 =>
          Sync[F].delay(standardOrder.compare(xSegment.last, ySegment.last))
        )
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
        Sync[F].delay(xSegment.length.compare(ySegment.length)).flatMap {
          case 0 =>
            // When the two chains have equal density, fallback to a rhoTestHash comparison on the latest blocks
            // in the density segments
            blake2b512Resource
              .use(implicit blake2b512 => Sync[F].delay(rhoTestHashOrderSlotData.compare(xSegment.last, ySegment.last)))
          case r => r.pure[F]
        }

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

  implicit private class OrderSupport[T](order: Order[T]) {

    def tiebreakWith(other: Order[T]): Order[T] =
      Order.whenEqual(order, other)
  }
}

/**
 * Typeclass indicating something is non-empty
 */
private trait NonEmpty[F[_]] {
  def head[A](c: F[A]): A
}

private object NonEmpty {

  def apply[F[_]: NonEmpty]: NonEmpty[F] = implicitly

  trait Instances {

    implicit val nonEmptyChainNonEmpty: NonEmpty[NonEmptyChain] =
      new NonEmpty[NonEmptyChain] {
        def head[A](c: NonEmptyChain[A]): A = c.head
      }

    implicit val nonEmptyVectorNonEmpty: NonEmpty[NonEmptyVector] =
      new NonEmpty[NonEmptyVector] {
        def head[A](c: NonEmptyVector[A]): A = c.head
      }
  }

  object instances extends Instances
}

/**
 * Typeclass indicating that something can prepend elements
 */
private trait Prepend[F[_]] {
  def prepend[A](a: A, c: F[A]): F[A]
}

private object Prepend {

  def apply[F[_]: Prepend]: Prepend[F] = implicitly

  trait Instances {

    implicit val nonEmptyChainPrepend: Prepend[NonEmptyChain] =
      new Prepend[NonEmptyChain] {
        def prepend[A](a: A, c: NonEmptyChain[A]): NonEmptyChain[A] = c.prepend(a)
      }

    implicit val nonEmptyVectorPrepend: Prepend[NonEmptyVector] =
      new Prepend[NonEmptyVector] {
        def prepend[A](a: A, c: NonEmptyVector[A]): NonEmptyVector[A] = c.prepend(a)
      }
  }

  object instances extends Instances
}
