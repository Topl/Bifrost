package co.topl.genus.interpreters

import akka.NotUsed
import cats.implicits._
import akka.stream.Materializer
import akka.stream.scaladsl.{Sink, Source}
import cats.Applicative
import cats.effect.kernel.Async
import co.topl.genus.algebras.MongoOplogAlg
import org.mongodb.scala.{Document, MongoClient, MongoCollection}
import org.mongodb.scala.bson.BsonTimestamp
import org.mongodb.scala.bson.conversions.Bson
import org.mongodb.scala.model.Filters

import scala.util.Try

object MongoOplogInterp {

  object Eval {

    def make[F[_]: Async](
      oplogCollection:       MongoCollection[Document]
    )(implicit materializer: Materializer): MongoOplogAlg[F] =
      new MongoOplogAlg[F] {

        override def getFirstDocTimestamp(databaseName: String, collectionName: String): F[Option[BsonTimestamp]] =
          getFirstTimestamp(
            Source
              .fromPublisher(
                oplogCollection
                  .find(Filters.eq("ns", s"$databaseName.$collectionName"))
              )
          )

        override def getFirstMatchingDocTimestamp(
          databaseName:   String,
          collectionName: String,
          matching:       Bson
        ): F[Option[BsonTimestamp]] =
          getFirstTimestamp(
            Source
              .fromPublisher(
                oplogCollection
                  .find(
                    Filters.and(
                      Filters.eq("ns", s"$databaseName.$collectionName"),
                      matching
                    )
                  )
              )
          )

        private def getFirstTimestamp(source: Source[Document, NotUsed]): F[Option[BsonTimestamp]] =
          Async[F].fromFuture(
            Async[F].delay(
              source
                .take(1)
                .flatMapConcat(document =>
                  document
                    .get("ts")
                    .flatMap(value => Try(value.asTimestamp()).toOption)
                    .fold[Source[BsonTimestamp, NotUsed]](Source.empty)(t => Source.single(t))
                )
                .runWith(Sink.fold[Option[BsonTimestamp], BsonTimestamp](None)((_, ts: BsonTimestamp) => Some(ts)))
            )
          )
      }
  }

  object Mock {

    def make[F[_]: Applicative](
      firstTimestamp:    Option[BsonTimestamp],
      matchingTimestamp: Option[BsonTimestamp]
    ): MongoOplogAlg[F] = new MongoOplogAlg[F] {

      override def getFirstDocTimestamp(databaseName: String, collectionName: String): F[Option[BsonTimestamp]] =
        firstTimestamp.pure[F]

      override def getFirstMatchingDocTimestamp(
        databaseName:   String,
        collectionName: String,
        matching:       Bson
      ): F[Option[BsonTimestamp]] =
        matchingTimestamp.pure[F]
    }
  }
}
