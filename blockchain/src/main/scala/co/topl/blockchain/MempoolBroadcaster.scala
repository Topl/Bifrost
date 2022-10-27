package co.topl.blockchain

import akka.stream.Materializer
import akka.stream.scaladsl.Source
import cats.effect.kernel.Async
import cats.implicits._
import co.topl.catsakka._
import co.topl.ledger.algebras.MempoolAlgebra
import co.topl.models.TypedIdentifier

object MempoolBroadcaster {

  def make[F[_]: Async](mempool: MempoolAlgebra[F])(implicit
    materializer:                Materializer
  ): F[(MempoolAlgebra[F], SourceMatNotUsed[TypedIdentifier])] =
    Async[F]
      .delay(Source.dropHeadQueue[F, TypedIdentifier](size = 64).preMaterialize())
      .map { case ((offer, _), source) =>
        val interpreter = new MempoolAlgebra[F] {
          def read(blockId: TypedIdentifier): F[Set[TypedIdentifier]] = mempool.read(blockId)

          def add(transactionId: TypedIdentifier): F[Unit] =
            mempool.add(transactionId) >> offer(transactionId)

          def remove(transactionId: TypedIdentifier): F[Unit] = mempool.remove(transactionId)
        }

        (interpreter, source)
      }
}
