package co.topl.genus.interpreters

import cats.implicits._
import akka.NotUsed
import akka.stream.scaladsl.Source
import cats.data.OptionT
import cats.effect.Async
import cats.{Applicative, MonadThrow}
import co.topl.genus.algebras.{DataStoreSubscriptionAlg, MongoOplogAlg}
import co.topl.genus.typeclasses.MongoFilter
import co.topl.genus.typeclasses.implicits._
import co.topl.genus.types.BlockHeight
import co.topl.utils.mongodb.DocumentDecoder
import org.mongodb.scala.MongoClient
import org.mongodb.scala.bson.BsonTimestamp
import org.mongodb.scala.model.{Aggregates, Filters}

object MongoSubscriptionInterp {

  type MongoSubscriptionAlg[F[_], T, Filter] =
    DataStoreSubscriptionAlg[F, Source[*, NotUsed], Filter, T]

  object Eval {

    def make[F[_]: Async: MonadThrow, T: DocumentDecoder, Filter: MongoFilter](
      mongoClient:    MongoClient,
      databaseName:   String,
      collectionName: String,
      heightPath:     String,
      oplog:          MongoOplogAlg[F]
    ): MongoSubscriptionAlg[F, T, Filter] =
      new MongoSubscriptionAlg[F, T, Filter] {

        override def fromStart(filter: Filter): F[Source[T, NotUsed]] =
          (for {
            startingTimestamp <-
              OptionT(oplog.getFirstDocTimestamp(databaseName, collectionName))
            stream = valuesFromTimestamp(startingTimestamp, filter)
          } yield stream)
            .fold[Source[T, NotUsed]](Source.empty)(source => source)

        override def fromCheckpoint(filter: Filter, checkpoint: BlockHeight): F[Source[T, NotUsed]] =
          (for {
            startingTimestamp <-
              OptionT(
                oplog.getFirstMatchingDocTimestamp(
                  databaseName,
                  collectionName,
                  Filters.eq(s"o.$heightPath", checkpoint)
                )
              )
            stream = valuesFromTimestamp(startingTimestamp, filter)
          } yield stream)
            .fold[Source[T, NotUsed]](Source.empty)(source => source)

        private def valuesFromTimestamp(timestamp: BsonTimestamp, filter: Filter): Source[T, NotUsed] =
          Source
            .fromPublisher(
              mongoClient
                .getDatabase(databaseName)
                .getCollection(collectionName)
                .watch(Seq(Aggregates.filter(filter.toBsonFilter)))
                .startAtOperationTime(timestamp)
            )
            .flatMapConcat(change =>
              DocumentDecoder[T]
                .fromDocument(change.getFullDocument)
                .fold(_ => Source.empty, t => Source.single(t))
            )
      }
  }

  object Mock {

    def make[F[_]: Applicative, T, Filter](
      fromStartResults:      List[T],
      fromCheckpointResults: List[T]
    ): MongoSubscriptionAlg[F, T, Filter] =
      new MongoSubscriptionAlg[F, T, Filter] {

        override def fromStart(filter: Filter): F[Source[T, NotUsed]] =
          Source(fromStartResults).pure[F]

        override def fromCheckpoint(filter: Filter, checkpoint: BlockHeight): F[Source[T, NotUsed]] =
          Source(fromCheckpointResults).pure[F]
      }
  }
}
