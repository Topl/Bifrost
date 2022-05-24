package co.topl.genus.algebras

import akka.NotUsed
import akka.stream.scaladsl.Source
import org.mongodb.scala.Document
import org.mongodb.scala.bson.conversions.Bson

/**
 * Interface for retrieving documents from a Mongo database.
 * @tparam F th effect-ful type to operate in
 */
trait MongoStore[F[_]] {

  /**
   * Gets the documents from Mongo DB with the given options.
   * @param filtering the filters to apply to the query
   * @param sorting the sorting to apply to the query
   * @param limit the number of documents to retrieve
   * @param skip the number of documents to skip
   * @return the queried documents
   */
  def getDocuments(
    filtering: Option[Bson],
    sorting:   Option[Bson],
    limit:     Option[Int],
    skip:      Option[Int]
  ): F[Source[Document, NotUsed]]
}
