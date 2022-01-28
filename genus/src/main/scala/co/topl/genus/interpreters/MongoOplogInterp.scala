package co.topl.genus.interpreters

import akka.NotUsed
import cats.implicits._
import akka.stream.Materializer
import akka.stream.scaladsl.{Sink, Source}
import cats.effect.kernel.Async
import co.topl.genus.algebras.MongoOplogAlg
import org.mongodb.scala.MongoClient
import org.mongodb.scala.bson.BsonTimestamp
import org.mongodb.scala.model.Filters

import scala.util.Try

object MongoOplogInterp {

  object Eval {

    def make[F[_]: Async](mongoClient: MongoClient)(implicit materializer: Materializer): MongoOplogAlg[F] =
      (databaseName: String, collectionName: String) =>
        Async[F]
          .fromFuture(
            Async[F].delay(
              Source
                .fromPublisher(
                  mongoClient
                    .getDatabase("local")
                    .getCollection("oplog.rs")
                    .find(Filters.eq("ns", s"$databaseName.$collectionName"))
                )
                .take(1)
                .flatMapConcat(document =>
                  document
                    .get("ts")
                    .flatMap(value => Try(value.asTimestamp()).toOption)
                    .fold[Source[BsonTimestamp, NotUsed]](Source.empty)(t => Source.single(t))
                )
                .runWith(Sink.seq)
            )
          )
          .map(_.headOption)
  }
}
