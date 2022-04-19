package co.topl.genus.interpreters.mongo

import akka.NotUsed
import akka.stream.scaladsl.Source
import cats.MonadThrow
import cats.data.OptionT
import cats.effect.Async
import co.topl.genus.algebras.{MongoOplog, MongoSubscription}
import co.topl.genus.typeclasses.MongoFilter
import co.topl.genus.typeclasses.implicits._
import co.topl.genus.types.BlockHeight
import co.topl.utils.mongodb.DocumentDecoder
import org.mongodb.scala.MongoClient
import org.mongodb.scala.bson.BsonTimestamp
import org.mongodb.scala.model.{Aggregates, Filters}

object MongoSubscriptionImpl {

  /**
   * Creates an interpreter of the [[MongoSubscription]] which operates on a Mongo database.
   *
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
  def make[F[_]: Async: MonadThrow, T: DocumentDecoder](
    mongoClient:    MongoClient,
    databaseName:   String,
    collectionName: String,
    heightPath:     String,
    oplog:          MongoOplog[F]
  ): MongoSubscription[F, T] = new Impl[F, T](mongoClient, databaseName, collectionName, heightPath, oplog)

  private class Impl[F[_]: Async: MonadThrow, T: DocumentDecoder](
    mongoClient:    MongoClient,
    databaseName:   String,
    collectionName: String,
    heightPath:     String,
    oplog:          MongoOplog[F]
  ) extends MongoSubscription[F, T] {

    override def fromStart[Filter: MongoFilter](filter: Filter): F[Source[T, NotUsed]] =
      (for {
        // find the timestamp of the first operation on the collection
        startingTimestamp <-
          OptionT(oplog.getFirstDocTimestamp(databaseName, collectionName))
        // subscribe to all new data inserted after the timestamp
        stream = valuesFromTimestamp(startingTimestamp, filter)
      } yield stream)
        // if no initial timestamp was found then the source will immediately complete with no data
        .fold[Source[T, NotUsed]](Source.empty)(source => source)

    override def fromBlockHeight[Filter: MongoFilter](filter: Filter, blockHeight: BlockHeight): F[Source[T, NotUsed]] =
      (for {
        // find the timestamp of the operation on the first document in the collection with a height of at least
        // the checkpoint value
        startingTimestamp <-
          OptionT(
            oplog.getFirstMatchingDocTimestamp(
              databaseName,
              collectionName,
              Filters.eq(s"o.$heightPath", blockHeight)
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
    private def valuesFromTimestamp[Filter: MongoFilter](
      timestamp: BsonTimestamp,
      filter:    Filter
    ): Source[T, NotUsed] =
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
