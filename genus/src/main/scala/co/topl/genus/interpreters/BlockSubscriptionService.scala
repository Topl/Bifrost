package co.topl.genus.interpreters

import akka.NotUsed
import akka.stream.scaladsl.Source
import cats.Id
import cats.implicits._
import co.topl.genus.algebras.DatabaseClientAlg
import co.topl.genus.filters.BlockFilter
import co.topl.genus.services.block_subscriptions.ReadBlockSubscriptionReq.{Nack, RequestType}
import co.topl.genus.services.block_subscriptions._
import co.topl.genus.types.Block

import java.util.UUID
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

object BlockSubscriptionService {

  object Eval {

    def make(databaseClient: DatabaseClientAlg[Id, Source[*, NotUsed]]): BlockSubscriptions =
      new BlockSubscriptions {
        override def create(in: CreateBlockSubscriptionReq): Future[CreateBlockSubscriptionRes] = ???

        override def delete(in: DeleteBlockSubscriptionReq): Future[DeleteBlockSubscriptionRes] = ???

        override def read(in: Source[ReadBlockSubscriptionReq, NotUsed]): Source[ReadBlockSubscriptionRes, NotUsed] =
          ???
      }
  }

  object Mock {

    def make: BlockSubscriptions =
      new BlockSubscriptions {

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
              case RequestType.Ack(_) =>
                Source.empty
              case RequestType.Nack(Nack(_, Nack.Reason.RETRY, _)) =>
                Source.single(
                  ReadBlockSubscriptionRes.of("mock-retry-id", Block().withId("mock-retry-block-id").some)
                )
              case RequestType.Nack(Nack(_, Nack.Reason.PAUSE, _)) => Source.empty
              case RequestType.Start(_) =>
                Source.tick(
                  3.seconds,
                  5.seconds,
                  ReadBlockSubscriptionRes.of("mock-id", Block().withId("mock-block-id").some)
                )
              case RequestType.Empty => Source.empty
            }
          )
      }
  }
}
