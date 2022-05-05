package co.topl.genus.interpreters.services

import akka.NotUsed
import akka.stream.scaladsl.Source
import cats.Functor
import cats.data.EitherT
import cats.implicits._
import co.topl.genus.algebras.{MongoSubscription, SubscriptionService}
import co.topl.genus.typeclasses.MongoFilter
import co.topl.genus.types.Block

object BlocksSubscriptionService {

  def make[F[_]: Functor](subscriptions: MongoSubscription[F]): SubscriptionService[F, Block] =
    new SubscriptionService[F, Block] {

      override def create[Filter: MongoFilter](
        request: SubscriptionService.CreateRequest[Filter]
      ): EitherT[F, SubscriptionService.CreateSubscriptionFailure, Source[Block, NotUsed]] =
        EitherT.right[SubscriptionService.CreateSubscriptionFailure](
          subscriptions
            .create(request.filter)
            .map(_.mapConcat(documentToBlock(_).toSeq))
        )
    }
}
