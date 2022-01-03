package co.topl.genus.interpreters

import cats.implicits._
import akka.NotUsed
import akka.stream.scaladsl.Source
import cats.{Id, Monad}
import co.topl.genus.algebras._
import co.topl.genus.filters._
import co.topl.genus.typeclasses.implicits._
import co.topl.genus.types.{Block, Transaction}
import com.mongodb.client.model.changestream.ChangeStreamDocument
import org.mongodb.scala.bson.conversions.Bson
import co.topl.utils.mongodb.models._
import org.mongodb.scala.Document

object MongoDatabaseClient {

  object Eval {

    def make[F[_]: Monad](
      transactionQuery: QueryAlg[F, Source[*, NotUsed], Bson, ConfirmedTransactionDataModel],
      blockQuery:       QueryAlg[F, Source[*, NotUsed], Bson, BlockDataModel],
      transactionSubscription: SubscriptionAlg[F, Source[*, NotUsed], Bson, String, ChangeStreamDocument[
        ConfirmedTransactionDataModel
      ]],
      blockSubscription: SubscriptionAlg[F, Source[*, NotUsed], Bson, String, ChangeStreamDocument[BlockDataModel]]
    ): DatabaseClientAlg[F, Source[*, NotUsed]] =
      new DatabaseClientAlg[F, Source[*, NotUsed]] {

        override def subscribeToTransactions(
          filter: TransactionFilter
        ): F[Source[Transaction, NotUsed]] =
          transactionSubscription
            .subscribe(filter.toFilter, None)
            .map(subscription => subscription.map(_.getFullDocument.transformTo))

        override def subscribeToBlocks(filter: BlockFilter): F[Source[Block, NotUsed]] =
          blockSubscription
            .subscribe(filter.toFilter, None)
            .map(subscription => subscription.map(_.getFullDocument.transformTo))

        override def queryTransactions(filter: TransactionFilter): F[Source[Transaction, NotUsed]] =
          transactionQuery
            .query(filter.toFilter)
            .map(query => query.map(_.transformTo))

        override def queryBlocks(filter: BlockFilter): F[Source[Block, NotUsed]] =
          blockQuery
            .query(filter.toFilter)
            .map(query => query.map(_.transformTo))
      }
  }
}
