package co.topl.genus.interpreters

import akka.NotUsed
import akka.stream.scaladsl.Source
import cats.Monad
import cats.implicits._
import co.topl.genus.algebras._
import co.topl.genus.filters._
import co.topl.genus.interpreters.MongoQuery.MongoQueryAlg
import co.topl.genus.interpreters.MongoSubscription.MongoSubscriptionAlg
import co.topl.genus.typeclasses.implicits._
import co.topl.genus.types.{Block, Transaction}
import co.topl.utils.mongodb.models._

object MongoDatabaseClient {

  object Eval {

    def make[F[_]: Monad](
      transactionQuery:        MongoQueryAlg[F, ConfirmedTransactionDataModel],
      blockQuery:              MongoQueryAlg[F, BlockDataModel],
      transactionSubscription: MongoSubscriptionAlg[F, ConfirmedTransactionDataModel],
      blockSubscription:       MongoSubscriptionAlg[F, BlockDataModel]
    ): DatabaseClientAlg[F, Source[*, NotUsed]] =
      new DatabaseClientAlg[F, Source[*, NotUsed]] {

        override def subscribeToTransactions(
          filter: TransactionFilter
        ): F[Source[Transaction, NotUsed]] =
          transactionSubscription
            .subscribe(filter.toBsonFilter, None)
            .map(subscription => subscription.map(_.getFullDocument.transformTo))

        override def subscribeToBlocks(filter: BlockFilter): F[Source[Block, NotUsed]] =
          blockSubscription
            .subscribe(filter.toBsonFilter, None)
            .map(subscription => subscription.map(_.getFullDocument.transformTo))

        override def queryTransactions(filter: TransactionFilter): F[Source[Transaction, NotUsed]] =
          transactionQuery
            .query(filter.toBsonFilter)
            .map(query => query.map(_.transformTo))

        override def queryBlocks(filter: BlockFilter): F[Source[Block, NotUsed]] =
          blockQuery
            .query(filter.toBsonFilter)
            .map(query => query.map(_.transformTo))
      }
  }
}
