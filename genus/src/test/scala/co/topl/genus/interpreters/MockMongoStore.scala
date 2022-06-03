package co.topl.genus.interpreters

import cats.implicits._
import akka.stream.scaladsl.Source
import cats.Applicative
import co.topl.genus.algebras.MongoStore
import co.topl.utils.mongodb.codecs._
import co.topl.utils.mongodb.implicits._
import co.topl.utils.mongodb.models.{BlockDataModel, ConfirmedTransactionDataModel}
import org.mongodb.scala.Document
import org.mongodb.scala.bson.conversions.Bson

object MockMongoStore {

  def withTransactions[F[_]: Applicative](transactions: List[ConfirmedTransactionDataModel]): MongoStore[F] =
    withDocuments(transactions.map(_.asDocument))

  def withBlocks[F[_]: Applicative](blocks: List[BlockDataModel]): MongoStore[F] =
    withDocuments(blocks.map(_.asDocument))

  def withDocuments[F[_]: Applicative](documents: List[Document]): MongoStore[F] =
    (_: Option[Bson], _: Option[Bson], _: Option[Int], _: Option[Int]) => Source(documents).pure[F]

  def inMemoryWithoutSortingOrFiltering[F[_]: Applicative](values: List[Document]): MongoStore[F] =
    (_: Option[Bson], _: Option[Bson], limit: Option[Int], skip: Option[Int]) =>
      Source(values.slice(skip.getOrElse(0), skip.getOrElse(0) + limit.getOrElse(values.length))).pure[F]
}
