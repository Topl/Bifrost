package co.topl.genus.interpreters

import akka.NotUsed
import akka.stream.alpakka.mongodb.scaladsl.MongoSource
import akka.stream.scaladsl.Source
import cats.Applicative
import cats.implicits._
import co.topl.genus.algebras.QueryAlg
import co.topl.utils.mongodb.{DocumentDecoder, DocumentEncoder}
import org.mongodb.scala.{Document, MongoCollection}
import org.mongodb.scala.bson.conversions.Bson

object MongoQuery {

  object Eval {

    def make[F[_]: Applicative, T: DocumentDecoder](
      collection: MongoCollection[Document]
    ): QueryAlg[F, Source[*, NotUsed], Bson, T] =
      (filter: Bson) =>
        MongoSource(collection.find(filter))
          .flatMapConcat(document =>
            DocumentDecoder[T]
              .fromDocument(document)
              .map(value => Source.single(value))
              .getOrElse(Source.empty)
          )
          .pure[F]
  }
}
