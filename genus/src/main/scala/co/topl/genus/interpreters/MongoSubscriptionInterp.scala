package co.topl.genus.interpreters

import akka.NotUsed
import akka.stream.alpakka.mongodb.scaladsl.MongoSource
import akka.stream.scaladsl.Source
import cats.Applicative
import cats.implicits._
import co.topl.genus.algebras.DataStoreSubscriptionAlg
import co.topl.genus.typeclasses.MongoFilter
import co.topl.genus.typeclasses.implicits._
import co.topl.utils.mongodb.DocumentDecoder
import org.mongodb.scala.model.{Aggregates, Filters}
import org.mongodb.scala.{Document, MongoClient}

object MongoSubscriptionInterp {

  type MongoSubscriptionAlg[F[_], T, Filter] =
    DataStoreSubscriptionAlg[F, Source[*, NotUsed], Filter, String, T]

  object Eval {

    def make[F[_]: Applicative, T: DocumentDecoder, Filter: MongoFilter](
      mongoClient:    MongoClient,
      databaseName:   String,
      collectionName: String
    ): MongoSubscriptionAlg[F, T, Filter] =
      (filter: Filter, lastSeenMessage: Option[String]) =>
        lastSeenMessage
          .map(message =>
            MongoSource(
              mongoClient
                .getDatabase(databaseName)
                .getCollection(collectionName)
                .watch(Seq(Aggregates.filter(filter.toBsonFilter)))
                .resumeAfter(Document("_data" -> message))
            )
          )
          .getOrElse(
            MongoSource(
              mongoClient
                .getDatabase("local")
                .getCollection("oplog.rs")
                .find(Filters.eq("ns", databaseName + "." + collectionName))
            )
              .take(1)
              .map(document => document.get("ts").map(_.asTimestamp()))
              .flatMapConcat(timestampOpt =>
                timestampOpt
                  .map(timestamp =>
                    MongoSource(
                      mongoClient
                        .getDatabase(databaseName)
                        .getCollection(collectionName)
                        .watch(Seq(Aggregates.filter(filter.toBsonFilter)))
                        .startAtOperationTime(timestamp)
                    )
                  )
                  .getOrElse(Source.empty)
              )
          )
          .flatMapConcat(change =>
            DocumentDecoder[T]
              .fromDocument(change.getFullDocument)
              .map(Source.single)
              .getOrElse(Source.empty)
          )
          .pure[F]
  }

  object Mock {

    def make[F[_]: Applicative, T, Filter]: MongoSubscriptionAlg[F, T, Filter] =
      (_: Filter, _: Option[String]) => Source.empty[T].pure[F]
  }
}
