package co.topl.genus

import akka.actor.ActorSystem
import cats.Functor
import cats.effect.{IO, IOApp}
import co.topl.genus.algebras.{MongoOplogAlg, SubscriptionServiceAlg}
import co.topl.genus.filters.TransactionFilter
import co.topl.genus.interpreters.MongoSubscriptionInterp.MongoSubscriptionAlg
import co.topl.genus.interpreters.{MongoOplogInterp, MongoSubscriptionInterp, SubscriptionServiceInterp}
import co.topl.genus.programs.TestSubProgram
import co.topl.genus.typeclasses.implicits._
import co.topl.genus.types.Transaction
import co.topl.utils.mongodb.codecs._
import co.topl.utils.mongodb.models.ConfirmedTransactionDataModel
import org.mongodb.scala.MongoClient

object TestSubApp extends IOApp.Simple {
  implicit val system: ActorSystem = ActorSystem("test")

  val defaultFilter: TransactionFilter = TransactionFilter(
    TransactionFilter.FilterType.All(TransactionFilter.AllFilter())
  )

  val mongoClient: MongoClient = MongoClient("mongodb://localhost:27017/?replicaset=bifrost")

  val mongoOpLog: MongoOplogAlg[IO] = MongoOplogInterp.Eval.make(mongoClient)

  val dataStore: MongoSubscriptionAlg[IO, Transaction, TransactionFilter] =
    Functor[MongoSubscriptionAlg[IO, *, TransactionFilter]]
      .map(
        MongoSubscriptionInterp.Eval
          .make[IO, ConfirmedTransactionDataModel, TransactionFilter](
            mongoClient,
            "chain_data",
            "confirmed_txes",
            mongoOpLog
          )
      )(_.transformTo[Transaction])

  val subservice: SubscriptionServiceAlg[IO, Transaction, TransactionFilter, Long] =
    SubscriptionServiceInterp.Eval.make[IO, Transaction, TransactionFilter](defaultFilter, dataStore)

  override def run: IO[Unit] =
    TestSubProgram.Eval
      .make(subservice)
      .guarantee(IO.fromFuture(IO.delay(system.terminate())).void)

}
