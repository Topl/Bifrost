package co.topl.genus.interpreters

import akka.NotUsed
import akka.stream.scaladsl.Source
import cats.Id
import cats.implicits._
import co.topl.genus.algebras.DatabaseClientAlg
import co.topl.genus.services.block_subscription.ReadBlockSubscriptionReq.RequestType
import co.topl.genus.services.block_subscription._
import co.topl.genus.types.Block

import java.util.UUID
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

object BlockSubscriptionService {

  object Mock {

    def make: BlockSubscription =
      new BlockSubscription {

        override def create(in: CreateBlockSubscriptionReq): Future[CreateBlockSubscriptionRes] =
          Future.successful(
            CreateBlockSubscriptionRes.of(UUID.randomUUID().toString)
          )

        override def delete(in: DeleteBlockSubscriptionReq): Future[DeleteBlockSubscriptionRes] =
          Future.successful(
            DeleteBlockSubscriptionRes.of()
          )

        override def read(in: Source[ReadBlockSubscriptionReq, NotUsed]): Source[ReadBlockSubscriptionRes, NotUsed] =
          in.flatMapConcat(request =>
            request.requestType match {
              case RequestType.Checkpoint(_) => Source.empty
              case RequestType.Pause(_)      => Source.empty
              case RequestType.Start(_) =>
                Source.tick(
                  3.seconds,
                  5.seconds,
                  ReadBlockSubscriptionRes.of(Block().withId("mock-block-id").some)
                )
              case RequestType.Empty => Source.empty
            }
          )
      }
  }
}
