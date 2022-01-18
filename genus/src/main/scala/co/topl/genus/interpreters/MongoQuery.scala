package co.topl.genus.interpreters

import akka.NotUsed
import akka.stream.alpakka.mongodb.scaladsl.MongoSource
import akka.stream.scaladsl.Source
import cats.implicits._
import cats.{Applicative, MonadError, MonadThrow}
import co.topl.genus.algebras.QueryAlg
import co.topl.utils.mongodb.DocumentDecoder
import org.mongodb.scala.bson.conversions.Bson
import org.mongodb.scala.{Document, MongoCollection}

object MongoQuery {

  type MongoQueryAlg[F[_], T] = QueryAlg[F, Source[*, NotUsed], Bson, T]

  object Eval {

    def make[F[_]: MonadThrow, T: DocumentDecoder](
      collection: MongoCollection[Document]
    ): MongoQueryAlg[F, T] =
      (filter: Bson) =>
        for {
          // handle potential error with connection to Mongo
          documentsSource <- MonadThrow[F].catchNonFatal(MongoSource(collection.find(filter)))
          querySource = documentsSource.flatMapConcat(document =>
            DocumentDecoder[T]
              .fromDocument(document)
              .map(Source.single)
              .getOrElse(Source.empty)
          )
        } yield querySource
  }

  object Mock {

    def make[F[_]: Applicative, T]: QueryAlg[F, Source[*, NotUsed], Bson, T] =
      (_: Bson) => Source.empty[T].pure[F]
  }
}
