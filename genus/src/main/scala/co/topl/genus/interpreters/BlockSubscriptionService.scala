package co.topl.genus.interpreters

import akka.NotUsed
import akka.stream.scaladsl.Source
import cats.implicits._
import co.topl.genus.services.blocks_subscription.ReadBlocksSubscriptionReq.RequestType
import co.topl.genus.services.blocks_subscription._
import co.topl.genus.types.Block

import java.util.UUID
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

object BlockSubscriptionService {

  object Mock {

    def make: BlocksSubscription =
      new BlocksSubscription {

        override def create(in: CreateBlocksSubscriptionReq): Future[CreateBlocksSubscriptionRes] =
          Future.successful(
            CreateBlocksSubscriptionRes.of(UUID.randomUUID().toString)
          )

        override def delete(in: DeleteBlocksSubscriptionReq): Future[DeleteBlocksSubscriptionRes] =
          Future.successful(
            DeleteBlocksSubscriptionRes.of()
          )

        override def read(in: Source[ReadBlocksSubscriptionReq, NotUsed]): Source[ReadBlocksSubscriptionRes, NotUsed] =
          in.flatMapConcat(request =>
            request.requestType match {
              case RequestType.Checkpoint(_) => Source.empty
              case RequestType.Pause(_)      => Source.empty
              case RequestType.Start(_) =>
                Source.tick(
                  3.seconds,
                  5.seconds,
                  ReadBlocksSubscriptionRes.of(Block().withId("mock-block-id").some)
                )
              case RequestType.Empty => Source.empty
            }
          )
      }
  }
}
