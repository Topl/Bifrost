package co.topl.genus.interpreters.services

import akka.NotUsed
import akka.stream.scaladsl.Source
import cats.MonadThrow
import cats.data.EitherT
import co.topl.genus.algebras.{MongoSubscription, SubscriptionService}
import co.topl.genus.services.blocks_query.BlockSorting
import co.topl.genus.typeclasses.MongoFilter
import co.topl.genus.typeclasses.implicits._
import co.topl.genus.types.Block

object BlocksSubscriptionService {

  def make[F[_]: MonadThrow](subscriptions: MongoSubscription[F]): SubscriptionService[F, Block] =
    new SubscriptionService[F, Block] {

      override def create[Filter: MongoFilter](
        request: SubscriptionService.CreateRequest[Filter]
      ): EitherT[F, SubscriptionService.CreateSubscriptionFailure, Source[Block, NotUsed]] =
        MonadThrow[F]
          // catch a possible failure with creating the subscription
          .attemptT(subscriptions.create(request.filter))
          .leftMap[SubscriptionService.CreateSubscriptionFailure](failure =>
            SubscriptionService.CreateSubscriptionFailures.DataConnectionFailure(failure.getMessage)
          )
          .map(source => source.mapConcat(documentToBlock(_).toSeq))
    }
}
