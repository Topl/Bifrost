package co.topl.transactiongenerator.interpreters

import cats.Traverse
import cats.implicits._
import cats.effect.Async
import cats.effect.std.Queue
import co.topl.models.Transaction
import co.topl.transactiongenerator.algebras.TransactionBroadcaster

object MultiTransactionBroadcaster {

  def make[F[_]: Async, G[_]: Traverse](delegates: G[TransactionBroadcaster[F]]) =
    for {
      queue <- Queue.unbounded[F, TransactionBroadcaster[F]]
      _     <- delegates.traverse(queue.offer)
    } yield new TransactionBroadcaster[F] {

      def broadcastTransaction(transaction: Transaction): F[Unit] =
        queue.take.flatMap(delegate =>
          delegate
            .broadcastTransaction(transaction)
            .flatTap(_ => queue.offer(delegate))
        )
    }

}
