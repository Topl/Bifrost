package co.topl.genus.programs

import akka.NotUsed
import akka.actor.ActorSystem
import akka.grpc.scaladsl.{ServerReflection, ServiceHandler}
import akka.http.scaladsl.Http
import akka.stream.scaladsl.Source
import cats.Id
import co.topl.genus.algebras.{DatabaseClientAlg, QueryAlg, SubscriptionAlg}
import co.topl.genus.interpreters._
import co.topl.genus.services.block_query.{BlockQuery, BlockQueryHandler}
import co.topl.genus.services.block_subscriptions._
import co.topl.genus.services.transaction_query.{TransactionQuery, TransactionQueryHandler}
import co.topl.genus.services.transaction_subscriptions.{TransactionSubscriptions, TransactionSubscriptionsHandler}
import co.topl.utils.mongodb.codecs._
import co.topl.utils.mongodb.models.{BlockDataModel, ConfirmedTransactionDataModel}
import org.mongodb.scala.MongoClient
import org.mongodb.scala.bson.conversions.Bson
import org.mongodb.scala.model.changestream.ChangeStreamDocument

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

object GenusProgram {

  object Eval {

    def make(
      actorSystem:           ActorSystem,
      mongoConnectionString: String,
      databaseName:          String,
      txCollectionName:      String,
      blockCollectionName:   String,
      ip:                    String,
      port:                  Int
    ): Future[Http.ServerBinding] = {
      import actorSystem._

      implicit val system: ActorSystem = actorSystem

      val mongoClient = MongoClient(mongoConnectionString)

      val txQuery: QueryAlg[Id, Source[*, NotUsed], Bson, ConfirmedTransactionDataModel] =
        MongoQuery.Eval.make(mongoClient.getDatabase(databaseName).getCollection(txCollectionName))

      val blockQuery: QueryAlg[Id, Source[*, NotUsed], Bson, BlockDataModel] =
        MongoQuery.Eval.make(mongoClient.getDatabase(databaseName).getCollection(blockCollectionName))

      val txSubscribe
        : SubscriptionAlg[Id, Source[*, NotUsed], Bson, ChangeStreamDocument[ConfirmedTransactionDataModel]] =
        MongoSubscription.Eval.make(mongoClient, databaseName, txCollectionName)

      val blockSubscribe: SubscriptionAlg[Id, Source[*, NotUsed], Bson, ChangeStreamDocument[BlockDataModel]] =
        MongoSubscription.Eval.make(mongoClient, databaseName, blockCollectionName)

      val mongoDatabaseClient: DatabaseClientAlg[Id, Source[*, NotUsed]] =
        MongoDatabaseClient.Eval
          .make(txQuery, blockQuery, txSubscribe, blockSubscribe)

      val blockSubscriptionService = BlockSubscriptionService.Eval.make(mongoDatabaseClient)

      val blockSubscriptionServiceHandler = BlockSubscriptionsHandler.partial(blockSubscriptionService)

      val reflectionServiceHandler = ServerReflection.partial(List(BlockSubscriptions))

      val serviceHandler = ServiceHandler.concatOrNotFound(blockSubscriptionServiceHandler, reflectionServiceHandler)

      Http(actorSystem)
        .newServerAt(ip, port)
        .bind(serviceHandler)
        .map(_.addToCoordinatedShutdown(hardTerminationDeadline = 10.seconds))
    }
  }

  object Mock {

    def make(actorSystem: ActorSystem, ip: String, port: Int): Future[Http.ServerBinding] = {
      implicit val system: ActorSystem = actorSystem
      import actorSystem._

      val txQueryHandler = TransactionQueryHandler.partial(TransactionQueryService.Mock.make)
      val blockQueryHandler = BlockQueryHandler.partial(BlockQueryService.Mock.make)
      val blockSubscriptionServiceHandler = BlockSubscriptionsHandler.partial(BlockSubscriptionService.Mock.make)
      val txSubscriptionServiceHandler =
        TransactionSubscriptionsHandler.partial(TransactionSubscriptionsService.Mock.make)

      val reflectionServiceHandler =
        ServerReflection.partial(
          List(
            TransactionQuery,
            BlockQuery,
            TransactionSubscriptions,
            BlockSubscriptions
          )
        )

      val serviceHandler =
        ServiceHandler.concatOrNotFound(
          txQueryHandler,
          blockQueryHandler,
          txSubscriptionServiceHandler,
          blockSubscriptionServiceHandler,
          reflectionServiceHandler
        )

      Http(actorSystem)
        .newServerAt(ip, port)
        .bind(serviceHandler)
        .map(_.addToCoordinatedShutdown(hardTerminationDeadline = 10.seconds))
    }
  }
}
