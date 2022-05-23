package co.topl.genus.interpreters

import akka.stream.scaladsl.Source
import cats.effect.IO
import co.topl.genus.algebras.MongoStore
import co.topl.utils.mongodb.models.{BlockDataModel, ConfirmedTransactionDataModel}
import org.mongodb.scala.Document
import org.mongodb.scala.bson.conversions.Bson
import co.topl.utils.mongodb.implicits._
import co.topl.utils.mongodb.codecs._

object MockMongoStore {

  def withTransactions(transactions: List[ConfirmedTransactionDataModel]): MongoStore[IO] =
    withDocuments(transactions.map(_.asDocument))

  def withBlocks(blocks: List[BlockDataModel]): MongoStore[IO] =
    withDocuments(blocks.map(_.asDocument))

  def withDocuments(documents: List[Document]): MongoStore[IO] =
    (_: Option[Bson], _: Option[Bson], _: Option[Int], _: Option[Int]) => IO(Source(documents))
}
