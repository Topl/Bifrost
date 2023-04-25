package co.topl.interpreters

import cats.Foldable
import cats.effect.Async
import cats.effect.std.Random
import cats.implicits._
import co.topl.algebras.{SynchronizationTraversalStep, ToplRpc}
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.consensus.models.BlockHeader
import co.topl.consensus.models.BlockId
import co.topl.node.models.BlockBody
import fs2.Stream

object MultiToplRpc {

  /**
   * Constructs an interpreter of `ToplRpc` that delegates requests to a collection
   * of sub-interpreters.  The delegate is chosen at random.
   * @param delegates a collection of ToplRpc interpreters
   * @tparam F an F-type constructor
   * @tparam G a collection type
   * @return a ToplRpc interpreter
   */
  def make[F[_]: Async, G[_]: Foldable](delegates: G[ToplRpc[F, Stream[F, *]]]): F[ToplRpc[F, Stream[F, *]]] =
    for {
      implicit0(random: Random[F]) <- Random.javaSecuritySecureRandom[F]
    } yield new ToplRpc[F, Stream[F, *]] {

      private val delegatesArray =
        delegates.toIterable.toArray

      private def randomDelegate: F[ToplRpc[F, Stream[F, *]]] =
        Random[F]
          .nextIntBounded(delegatesArray.length)
          .map(delegatesArray(_))

      def broadcastTransaction(transaction: IoTransaction): F[Unit] =
        randomDelegate.flatMap(_.broadcastTransaction(transaction))

      def currentMempool(): F[Set[TransactionId]] =
        randomDelegate.flatMap(_.currentMempool())

      def fetchBlockHeader(blockId: BlockId): F[Option[BlockHeader]] =
        randomDelegate.flatMap(_.fetchBlockHeader(blockId))

      def fetchBlockBody(blockId: BlockId): F[Option[BlockBody]] =
        randomDelegate.flatMap(_.fetchBlockBody(blockId))

      def fetchTransaction(transactionId: TransactionId): F[Option[IoTransaction]] =
        randomDelegate.flatMap(_.fetchTransaction(transactionId))

      def blockIdAtHeight(height: Long): F[Option[BlockId]] =
        randomDelegate.flatMap(_.blockIdAtHeight(height))

      def blockIdAtDepth(depth: Long): F[Option[BlockId]] =
        randomDelegate.flatMap(_.blockIdAtDepth(depth))

      override def synchronizationTraversal(): F[Stream[F, SynchronizationTraversalStep]] =
        randomDelegate.flatMap(_.synchronizationTraversal())
    }
}
