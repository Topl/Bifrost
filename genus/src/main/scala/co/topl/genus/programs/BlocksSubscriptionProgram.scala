package co.topl.genus.programs

import akka.NotUsed
import akka.stream.scaladsl.Source
import cats.effect.kernel.Async
import cats.~>
import co.topl.genus.algebras.SubscriptionServiceAlg
import co.topl.genus.filters.BlockFilter
import co.topl.genus.ops.implicits._
import co.topl.genus.services.blocks_subscription.{
  BlocksSubscription,
  BlocksSubscriptionRes,
  CreateBlocksSubscriptionReq
}
import co.topl.genus.types.{Block, BlockHeight}
import co.topl.genus.typeclasses.implicits._

import scala.concurrent.Future

object BlocksSubscriptionProgram {

  object Eval {

    def make[F[_]: Async: *[_] ~> Future](
      subsService: SubscriptionServiceAlg[F, Block, BlockFilter]
    ): BlocksSubscription =
      (in: CreateBlocksSubscriptionReq) =>
        Source
          .futureSource(
            subsService
              .create(in.toRequest)
              .fold(
                failure => Source.single(BlocksSubscriptionRes.fromCreateFailure(failure)),
                blocks => BlocksSubscriptionRes.fromBlocks[Source[*, NotUsed]](blocks)
              )
              .mapFunctor
          )
          .mapMaterializedValue(_ => NotUsed)
  }
}
