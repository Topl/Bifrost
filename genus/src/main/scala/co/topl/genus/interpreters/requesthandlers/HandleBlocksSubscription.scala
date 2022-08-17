package co.topl.genus.interpreters.requesthandlers

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.scaladsl.Source
import cats.effect.Async
import cats.{~>, Monad}
import co.topl.genus.algebras.SubscriptionService
import co.topl.genus.ops.implicits._
import co.topl.genus.services.blocks_subscription._
import co.topl.genus.typeclasses.implicits._
import co.topl.genus.types.Block

import scala.concurrent.Future

object HandleBlocksSubscription {

  def make[F[_]: Async: Monad: *[_] ~> Future](
    subscriptions:   SubscriptionService[F, Block]
  )(implicit system: ActorSystem): BlocksSubscription =
    (in: CreateBlocksSubscriptionReq) =>
      Source
        .futureSource(
          subscriptions
            .create(in.toRequest)
            .fold(
              failure => Source.single(BlocksSubscriptionRes.fromCreateFailure(failure)),
              blocks => BlocksSubscriptionRes.fromBlocks[Source[*, NotUsed]](blocks)
            )
            .mapFunctor
        )
        .mapMaterializedValue(_ => NotUsed)
}
