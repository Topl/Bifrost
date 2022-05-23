package co.topl.genus.interpreters

import akka.NotUsed
import akka.stream.scaladsl.Source
import cats.implicits._
import cats.{Applicative, MonadThrow}
import co.topl.genus.algebras.MongoSubscription
import co.topl.genus.typeclasses.{MongoFilter, MongoSort}
import org.mongodb.scala.Document

object MockMongoSubscription {

  def alwaysFailWith[F[_]: MonadThrow](message: String): MongoSubscription[F] =
    new MongoSubscription[F] {

      override def create[Filter: MongoFilter, Sort: MongoSort](
        filter:            Filter,
        sort:              Sort,
        confirmationDepth: Int
      ): F[Source[Document, NotUsed]] = MonadThrow[F].raiseError(new Throwable(message))
    }

  def withDocuments[F[_]: Applicative](documents: List[Document]): MongoSubscription[F] =
    new MongoSubscription[F] {

      override def create[Filter: MongoFilter, Sort: MongoSort](
        filter:            Filter,
        sort:              Sort,
        confirmationDepth: Int
      ): F[Source[Document, NotUsed]] =
        Source(documents).pure[F]
    }
}
