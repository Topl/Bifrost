package co.topl.blockchain

import cats.data.EitherT
import cats.effect.Resource
import cats.effect.kernel.Async
import cats.implicits._
import co.topl.brambl.models.Identifier
import co.topl.consensus.models.BlockId
import co.topl.ledger.algebras.MempoolAlgebra
import fs2.concurrent.Topic

object MempoolBroadcaster {

  def make[F[_]: Async](
    mempool: MempoolAlgebra[F]
  ): Resource[F, (MempoolAlgebra[F], Topic[F, Identifier.IoTransaction32])] =
    Resource
      .make(Topic[F, Identifier.IoTransaction32])(_.close.void)
      .map { topic =>
        val interpreter =
          new MempoolAlgebra[F] {
            def read(blockId: BlockId): F[Set[Identifier.IoTransaction32]] = mempool.read(blockId)

            def add(transactionId: Identifier.IoTransaction32): F[Unit] =
              mempool.add(transactionId) >>
              EitherT(topic.publish1(transactionId))
                .leftMap(_ => new IllegalStateException("MempoolBroadcaster topic unexpectedly closed"))
                .rethrowT

            def remove(transactionId: Identifier.IoTransaction32): F[Unit] = mempool.remove(transactionId)
          }

        (interpreter, topic)
      }

}
