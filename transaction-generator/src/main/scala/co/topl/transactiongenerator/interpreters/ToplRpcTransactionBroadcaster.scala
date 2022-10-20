package co.topl.transactiongenerator.interpreters

import cats.effect._
import cats.implicits._
import co.topl.algebras._
import co.topl.models.Transaction
import co.topl.transactiongenerator.algebras._

object ToplRpcTransactionBroadcaster {

  def make[F[_]: Sync](toplRpc: ToplRpc[F]): F[TransactionBroadcaster[F]] =
    Sync[F].delay {
      new TransactionBroadcaster[F] {
        def broadcastTransaction(transaction: Transaction): F[Unit] =
          toplRpc.broadcastTransaction(transaction)
      }
    }
}
