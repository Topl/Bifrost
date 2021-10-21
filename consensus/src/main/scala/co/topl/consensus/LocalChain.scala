package co.topl.consensus

import cats.data.Validated
import cats.effect.Ref
import cats.effect.kernel.Sync
import cats.implicits._
import co.topl.typeclasses.OrderT

/**
 * Operations involving this node's locally-adopted canonical chain
 */
trait LocalChain[F[_]] {

  /**
   * Indicates if the provided "head" results in a better chain than the current local chain.
   *
   * The `newHead` _can_ be invalid.  (For example, the block needs to have the proper syntax,
   * but it may not necessarily need to be validated for consensus and ledger purposes)
   *
   * @param newHead The head of a new tine, either from a network peer or from a local staker
   * @return True if the provided segment is better than the local canonical chain
   */
  def isWorseThan(newHead: SlotData): F[Boolean]

  /**
   * Instructs the node to adopt the given canonical head.  This head, along with all of its ancestors, should be
   * pre-validated elsewhere.
   *
   * @param newHead The new _valid_ canonical head slot to adopt
   */
  def adopt(newHead: Validated.Valid[SlotData]): F[Unit]

  /**
   * The head of the chain that has been adopted locally by this node.
   */
  def head: F[SlotData]
}

object LocalChain {

  object Eval {

    def make[F[_]: Sync](
      initialHead:    SlotData,
      chainSelection: OrderT[F, SlotData]
    ): F[LocalChain[F]] =
      Ref
        .of[F, SlotData](initialHead)
        .map(headRef =>
          new LocalChain[F] {

            def isWorseThan(newHead: SlotData): F[Boolean] =
              head.flatMap(chainSelection.compare(_, newHead).map(_ < 0))

            def adopt(newHead: Validated.Valid[SlotData]): F[Unit] =
              headRef.update(_ => newHead.a)

            val head: F[SlotData] =
              headRef.get
          }
        )
  }
}
