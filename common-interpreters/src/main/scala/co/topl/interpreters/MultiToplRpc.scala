package co.topl.interpreters

import cats.Foldable
import cats.effect.Async
import cats.effect.std.Random
import cats.implicits._
import co.topl.algebras.{SynchronizationTraversalStep, ToplRpc}
import co.topl.{models => legacyModels}
import legacyModels.TypedIdentifier
import co.topl.consensus.models.BlockHeader
import co.topl.node.models.BlockBody
import co.topl.proto.models.Transaction
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

      def broadcastTransaction(transaction: Transaction): F[Unit] =
        randomDelegate.flatMap(_.broadcastTransaction(transaction))

      def currentMempool(): F[Set[TypedIdentifier]] =
        randomDelegate.flatMap(_.currentMempool())

      def fetchBlockHeader(blockId: TypedIdentifier): F[Option[BlockHeader]] =
        randomDelegate.flatMap(_.fetchBlockHeader(blockId))

      def fetchBlockBody(blockId: TypedIdentifier): F[Option[BlockBody]] =
        randomDelegate.flatMap(_.fetchBlockBody(blockId))

      def fetchTransaction(transactionId: TypedIdentifier): F[Option[Transaction]] =
        randomDelegate.flatMap(_.fetchTransaction(transactionId))

      def blockIdAtHeight(height: Long): F[Option[TypedIdentifier]] =
        randomDelegate.flatMap(_.blockIdAtHeight(height))

      def blockIdAtDepth(depth: Long): F[Option[TypedIdentifier]] =
        randomDelegate.flatMap(_.blockIdAtDepth(depth))

      override def synchronizationTraversal(): F[Stream[F, SynchronizationTraversalStep]] =
        randomDelegate.flatMap(_.synchronizationTraversal())
    }
}
