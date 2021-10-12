package co.topl.consensus

import cats.effect.Ref
import cats.effect.kernel.Sync
import cats.implicits._
import co.topl.typeclasses.OrderT

/**
 * Operations involving this node's locally-adopted canonical chain
 */
trait LocalChain[F[_]] {

  /**
   * Indicates if the provided "head" results in a better chain than the current local chain
   * @param newHead The head of a new tine, either from a network peer or from a local staker
   * @return True if the provided segment is better than the local canonical chain
   */
  def isWorseThan(newHead: SlotData): F[Boolean]

  /**
   * Instructs the node to adopt the given canonical head.  This head, along with all of its ancestors, should be
   * pre-validated elsewhere
   * @param newHead The new canonical head slot to adopt
   */
  def adopt(newHead: SlotData): F[Unit]

  /**
   * The head of the chain that has been adopted locally by this node.
   */
  def current: F[SlotData]
}

object LocalChain {

  object Eval {

    def make[F[_]: Sync](
      initialHead:    SlotData,
      chainSelection: OrderT[F, SlotData]
    ): F[LocalChain[F]] =
      Ref
        .of[F, SlotData](initialHead)
        .map(head =>
          new LocalChain[F] {

            def isWorseThan(newHead: SlotData): F[Boolean] =
              current.flatMap(chainSelection.compare(_, newHead).map(_ < 0))

            def adopt(newHead: SlotData): F[Unit] =
              head.update(_ => newHead)

            val current: F[SlotData] =
              Sync[F].defer(head.get)
          }
        )
  }
}
