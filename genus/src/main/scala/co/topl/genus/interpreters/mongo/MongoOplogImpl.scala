package co.topl.genus.interpreters.mongo

import akka.NotUsed
import akka.stream.Materializer
import akka.stream.scaladsl.{Sink, Source}
import cats.Applicative
import cats.effect.kernel.Async
import co.topl.genus.algebras.MongoOplog
import org.mongodb.scala.bson.BsonTimestamp
import org.mongodb.scala.bson.conversions.Bson
import org.mongodb.scala.model.Filters
import org.mongodb.scala.{Document, MongoCollection}

import scala.util.Try

object MongoOplogImpl {

  /**
   * Creates an interpreter which can be used to get data from the Mongo oplog.
   *
   * @param oplogCollection the Mongo oplog collection
   * @param materializer a stream materializer
   * @tparam F the effect-ful type of the final value in the program
   * @return an instance of the [[MongoOplog]]
   */
  def make[F[_]: Async](
    oplogCollection:       MongoCollection[Document]
  )(implicit materializer: Materializer): MongoOplog[F] = new Impl[F](oplogCollection)

  private class Impl[F[_]: Async](oplogCollection: MongoCollection[Document])(implicit materializer: Materializer)
      extends MongoOplog[F] {

    override def getFirstDocTimestamp(databaseName: String, collectionName: String): F[Option[BsonTimestamp]] =
      getFirstTimestamp(
        Source
          .fromPublisher(
            oplogCollection
              // the "ns" value represents the path to the collection and can be used to filter on
              // operations from a specific collection
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
            // find the operations on the collection that also match the given filter
            oplogCollection
              .find(
                Filters.and(
                  Filters.eq("ns", s"$databaseName.$collectionName"),
                  matching
                )
              )
          )
      )

    /**
     * Gets the timestamp of the first document in the stream.
     * @param source the source to get the first value from
     * @return the timestamp if at least one document exists
     */
    private def getFirstTimestamp(source: Source[Document, NotUsed]): F[Option[BsonTimestamp]] =
      Async[F].fromFuture(
        Async[F].delay(
          source
            // get the first document from the source
            .take(1)
            .flatMapConcat(document =>
              // the "ts" key in an oplog document contains the timestamp data
              document
                .get("ts")
                .flatMap(value => Try(value.asTimestamp()).toOption)
                // if the timestamp value is found then return it, otherwise return nothing
                .fold[Source[BsonTimestamp, NotUsed]](Source.empty)(t => Source.single(t))
            )
            // get the found timestamp if it exists
            .runWith(Sink.fold[Option[BsonTimestamp], BsonTimestamp](None)((_, ts: BsonTimestamp) => Some(ts)))
        )
      )
  }
}
