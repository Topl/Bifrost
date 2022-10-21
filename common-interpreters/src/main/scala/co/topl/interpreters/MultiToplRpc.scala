package co.topl.interpreters

import cats.Foldable
import cats.effect.Async
import cats.effect.std.Random
import cats.implicits._
import co.topl.algebras.ToplRpc
import co.topl.models.{BlockBodyV2, BlockHeaderV2, Transaction, TypedIdentifier}

object MultiToplRpc {

  /**
   * Constructs an interpreter of `ToplRpc` that delegates requests to a collection
   * of sub-interpreters.
   * @param delegates a collection of ToplRpc interpreters
   * @tparam F an F-type constructor
   * @tparam G a collection type
   * @return a ToplRpc interpreter
   */
  def make[F[_]: Async, G[_]: Foldable](delegates: G[ToplRpc[F]]): F[ToplRpc[F]] =
    for {
      implicit0(random: Random[F]) <- Random.javaSecuritySecureRandom[F]
    } yield new ToplRpc[F] {

      private val delegatesArray =
        delegates.toIterable.toArray

      private def randomDelegate: F[ToplRpc[F]] =
        Random[F]
          .nextIntBounded(delegatesArray.length)
          .map(delegatesArray(_))

      def broadcastTransaction(transaction: Transaction): F[Unit] =
        randomDelegate.flatMap(_.broadcastTransaction(transaction))

      def currentMempool(): F[Set[TypedIdentifier]] =
        randomDelegate.flatMap(_.currentMempool())

      def fetchBlockHeader(blockId: TypedIdentifier): F[Option[BlockHeaderV2]] =
        randomDelegate.flatMap(_.fetchBlockHeader(blockId))

      def fetchBlockBody(blockId: TypedIdentifier): F[Option[BlockBodyV2]] =
        randomDelegate.flatMap(_.fetchBlockBody(blockId))

      def fetchTransaction(transactionId: TypedIdentifier): F[Option[Transaction]] =
        randomDelegate.flatMap(_.fetchTransaction(transactionId))

      def blockIdAtHeight(height: Long): F[Option[TypedIdentifier]] =
        randomDelegate.flatMap(_.blockIdAtHeight(height))

      def blockIdAtDepth(depth: Long): F[Option[TypedIdentifier]] =
        randomDelegate.flatMap(_.blockIdAtDepth(depth))
    }
}
