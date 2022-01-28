package co.topl.genus.programs

import cats.implicits._
import akka.stream.Materializer
import cats.data.EitherT
import cats.effect.kernel.Async
import co.topl.genus.algebras.SubscriptionServiceAlg
import co.topl.genus.algebras.SubscriptionServiceAlg.{CreateRequest, CreateSubscriptionFailure}
import co.topl.genus.filters.TransactionFilter
import co.topl.genus.types.Transaction

object TestSubProgram {

  object Eval {

    def make[F[_]: Async](
      subService:            SubscriptionServiceAlg[F, Transaction, TransactionFilter, Long]
    )(implicit materializer: Materializer): F[Unit] =
      (for {
        sub <- subService.create(CreateRequest[TransactionFilter, Long](None, None, 0))
        result <- EitherT.right[CreateSubscriptionFailure](
          Async[F].fromFuture(Async[F].delay(sub.runForeach(tx => println(tx.blockHeight))))
        )
      } yield result)
        .map(_ => println("completed stream successfully"))
        .value
        .void
  }
}
