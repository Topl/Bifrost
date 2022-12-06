package co.topl.blockchain

import cats.effect.kernel.Async
import cats.implicits._
import co.topl.ledger.algebras.MempoolAlgebra
import co.topl.models.TypedIdentifier
import fs2.concurrent.Topic

object MempoolBroadcaster {

  def make[F[_]: Async](
    mempool:           MempoolAlgebra[F],
    txsAdoptionsTopic: Topic[F, TypedIdentifier]
  ): F[MempoolAlgebra[F]] =
    Async[F].delay {
      new MempoolAlgebra[F] {
        def read(blockId: TypedIdentifier): F[Set[TypedIdentifier]] = mempool.read(blockId)

        def add(transactionId: TypedIdentifier): F[Unit] =
          mempool.add(transactionId) >>
          txsAdoptionsTopic.publish1(transactionId).map(_.leftMap(_ => ())).map(_.merge)

        def remove(transactionId: TypedIdentifier): F[Unit] = mempool.remove(transactionId)
      }
    }

}
