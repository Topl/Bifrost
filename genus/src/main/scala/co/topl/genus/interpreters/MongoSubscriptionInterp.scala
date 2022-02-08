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

    /**
     * Creates an interpreter of the [[DataStoreSubscriptionAlg]] which operates on a Mongo database.
     * @param mongoClient the mongo client containing the mongo connection
     * @param databaseName the name of the database containing the collection to subscribe to
     * @param collectionName the name of the collection to subscribe to
     * @param heightPath the path to the block height value inside the mongo document
     * @param oplog operations for interacting with the mongo oplog collection
     * @tparam F the effect-ful type of the final value in the program
     * @tparam T the type of data that can be subscribed to
     * @tparam Filter the type of filter that can be applied
     * @return a new instance of [[MongoSubscriptionAlg]]
     */
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
            // find the timestamp of the first operation on the collection
            startingTimestamp <-
              OptionT(oplog.getFirstDocTimestamp(databaseName, collectionName))
            // subscribe to all new data inserted after the timestamp
            stream = valuesFromTimestamp(startingTimestamp, filter)
          } yield stream)
            // if no initial timestamp was found then the source will immediately complete with no data
            .fold[Source[T, NotUsed]](Source.empty)(source => source)

        override def fromCheckpoint(filter: Filter, checkpoint: BlockHeight): F[Source[T, NotUsed]] =
          (for {
            // find the timestamp of the operation on the first document in the collection with a height of at least
            // the checkpoint value
            startingTimestamp <-
              OptionT(
                oplog.getFirstMatchingDocTimestamp(
                  databaseName,
                  collectionName,
                  Filters.eq(s"o.$heightPath", checkpoint)
                )
              )
            // subscribe to all new data inserted after the timestamp
            stream = valuesFromTimestamp(startingTimestamp, filter)
          } yield stream)
            // if no initial timestamp was found then the source will immediately complete with no data
            .fold[Source[T, NotUsed]](Source.empty)(source => source)

        /**
         * Subscribes to all values that have occurred in a Mongo collection at or beyond the given timestamp.
         * @param timestamp the starting timestamp
         * @param filter a filter to apply to the set of subscribed data
         * @return a [[Source]] of subscribed data
         */
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
              // convert the change document into a value of T
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
