package co.topl.genus.algebras

import akka.NotUsed
import akka.stream.scaladsl.Source
import org.mongodb.scala.Document
import org.mongodb.scala.bson.conversions.Bson

trait MongoStore[F[_]] {

  def getDocuments(
    filtering: Option[Bson],
    sorting:   Option[Bson],
    limit:     Option[Int],
    skip:      Option[Int]
  ): F[Source[Document, NotUsed]]
}
