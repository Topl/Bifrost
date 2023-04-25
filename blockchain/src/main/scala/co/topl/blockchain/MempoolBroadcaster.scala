package co.topl.blockchain

import cats.data.EitherT
import cats.effect.Resource
import cats.effect.kernel.Async
import cats.implicits._
import co.topl.brambl.models.TransactionId
import co.topl.consensus.models.BlockId
import co.topl.ledger.algebras.MempoolAlgebra
import fs2.concurrent.Topic

object MempoolBroadcaster {

  def make[F[_]: Async](
    mempool: MempoolAlgebra[F]
  ): Resource[F, (MempoolAlgebra[F], Topic[F, TransactionId])] =
    Resource
      .make(Topic[F, TransactionId])(_.close.void)
      .map { topic =>
        val interpreter =
          new MempoolAlgebra[F] {
            def read(blockId: BlockId): F[Set[TransactionId]] = mempool.read(blockId)

            def add(transactionId: TransactionId): F[Unit] =
              mempool.add(transactionId) >>
              EitherT(topic.publish1(transactionId))
                .leftMap(_ => new IllegalStateException("MempoolBroadcaster topic unexpectedly closed"))
                .rethrowT

            def remove(transactionId: TransactionId): F[Unit] = mempool.remove(transactionId)
          }

        (interpreter, topic)
      }

}
