package co.topl.interpreters

import cats.Foldable
import cats.effect.Async
import cats.effect.std.Random
import cats.effect.std.SecureRandom
import cats.implicits._
import co.topl.algebras.NodeRpc
import co.topl.algebras.SynchronizationTraversalStep
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.consensus.models.BlockHeader
import co.topl.consensus.models.BlockId
import co.topl.models.Epoch
import co.topl.node.models.BlockBody
import co.topl.proto.node.{EpochData, NodeConfig}
import fs2.Stream

object MultiNodeRpc {

  /**
   * Constructs an interpreter of `ToplRpc` that delegates requests to a collection
   * of sub-interpreters.  The delegate is chosen at random.
   * @param delegates a collection of ToplRpc interpreters
   * @tparam F an F-type constructor
   * @tparam G a collection type
   * @return a ToplRpc interpreter
   */
  def make[F[_]: Async, G[_]: Foldable](delegates: G[NodeRpc[F, Stream[F, *]]]): F[NodeRpc[F, Stream[F, *]]] =
    for {
      implicit0(random: Random[F]) <- SecureRandom.javaSecuritySecureRandom[F]
    } yield new NodeRpc[F, Stream[F, *]] {

      private val delegatesArray =
        delegates.toIterable.toArray

      private def randomDelegate: F[NodeRpc[F, Stream[F, *]]] =
        Random[F]
          .nextIntBounded(delegatesArray.length)
          .map(delegatesArray(_))

      def broadcastTransaction(transaction: IoTransaction): F[Unit] =
        randomDelegate.flatMap(_.broadcastTransaction(transaction))

      def currentMempool(): F[Set[TransactionId]] =
        randomDelegate.flatMap(_.currentMempool())

      def currentMempoolContains(transactionId: TransactionId): F[Boolean] =
        randomDelegate.flatMap(_.currentMempoolContains(transactionId))

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

      def synchronizationTraversal(): F[Stream[F, SynchronizationTraversalStep]] =
        randomDelegate.flatMap(_.synchronizationTraversal())

      def fetchProtocolConfigs(): F[Stream[F, NodeConfig]] =
        randomDelegate.flatMap(_.fetchProtocolConfigs())

      def fetchEpochData(epoch: Epoch): F[Option[EpochData]] =
        randomDelegate.flatMap(_.fetchEpochData(epoch))
    }
}
