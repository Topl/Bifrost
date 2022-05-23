package co.topl.genus.interpreters

import akka.NotUsed
import akka.stream.scaladsl.Source
import cats.effect.IO
import co.topl.genus.algebras.MongoSubscription
import co.topl.genus.typeclasses.{MongoFilter, MongoSort}
import org.mongodb.scala.Document

object MockMongoSubscription {

  def alwaysFailWith(message: String): MongoSubscription[IO] =
    new MongoSubscription[IO] {

      override def create[Filter: MongoFilter, Sort: MongoSort](
        filter:            Filter,
        sort:              Sort,
        confirmationDepth: Int
      ): IO[Source[Document, NotUsed]] = IO.raiseError(new Throwable(message))
    }

  def withDocuments(documents: List[Document]): MongoSubscription[IO] =
    new MongoSubscription[IO] {

      override def create[Filter: MongoFilter, Sort: MongoSort](
        filter:            Filter,
        sort:              Sort,
        confirmationDepth: Int
      ): IO[Source[Document, NotUsed]] =
        IO(Source(documents))
    }
}
