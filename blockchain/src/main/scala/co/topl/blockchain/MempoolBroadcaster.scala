package co.topl.blockchain

import cats.data.EitherT
import cats.effect.Resource
import cats.effect.kernel.Async
import cats.implicits._
import co.topl.ledger.algebras.MempoolAlgebra
import co.topl.models.TypedIdentifier
import fs2.concurrent.Topic

object MempoolBroadcaster {

  def make[F[_]: Async](mempool: MempoolAlgebra[F]): Resource[F, (MempoolAlgebra[F], Topic[F, TypedIdentifier])] =
    Resource
      .make(Topic[F, TypedIdentifier])(_.close.void)
      .map { topic =>
        val interpreter =
          new MempoolAlgebra[F] {
            def read(blockId: TypedIdentifier): F[Set[TypedIdentifier]] = mempool.read(blockId)

            def add(transactionId: TypedIdentifier): F[Unit] =
              mempool.add(transactionId) >>
              EitherT(topic.publish1(transactionId))
                .leftMap(_ => new IllegalStateException("MempoolBroadcaster topic unexpectedly closed"))
                .rethrowT

            def remove(transactionId: TypedIdentifier): F[Unit] = mempool.remove(transactionId)
          }

        (interpreter, topic)
      }

}
