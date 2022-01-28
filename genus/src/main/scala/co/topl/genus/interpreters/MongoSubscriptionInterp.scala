package co.topl.genus.interpreters

import akka.NotUsed
import akka.stream.scaladsl.Source
import cats.data.OptionT
import cats.effect.Async
import cats.{Applicative, MonadThrow}
import co.topl.genus.algebras.{DataStoreSubscriptionAlg, MongoOplogAlg}
import co.topl.genus.interpreters.MongoSubscriptionInterp.MongoSubscriptionAlg
import co.topl.genus.typeclasses.MongoFilter
import co.topl.genus.typeclasses.implicits._
import co.topl.utils.mongodb.DocumentDecoder
import org.mongodb.scala.model.Aggregates
import org.mongodb.scala.model.changestream.ChangeStreamDocument
import org.mongodb.scala.{Document, MongoClient}

object MongoSubscriptionInterp {

  type MongoSubscriptionAlg[F[_], T, Filter] =
    DataStoreSubscriptionAlg[F, Source[*, NotUsed], Filter, Long, T]

  object Eval {

    def make[F[_]: Async: MonadThrow, T: DocumentDecoder, Filter: MongoFilter](
      mongoClient:    MongoClient,
      databaseName:   String,
      collectionName: String,
      oplog:          MongoOplogAlg[F]
    ): MongoSubscriptionAlg[F, T, Filter] =
      new MongoSubscriptionAlg[F, T, Filter] {

        override def fromStart(filter: Filter): F[Source[T, NotUsed]] =
          (for {
            startingTimestamp <-
              OptionT(oplog.getFirstDocumentTimestamp(databaseName, collectionName))
            changeStream =
              Source.fromPublisher(
                mongoClient
                  .getDatabase(databaseName)
                  .getCollection(collectionName)
                  .watch(Seq(Aggregates.filter(filter.toBsonFilter)))
                  .startAtOperationTime(startingTimestamp)
              )
            documents = fromChangeStreamSource[NotUsed](changeStream)
          } yield documents)
            .fold[Source[T, NotUsed]](Source.empty)(source => source)

        override def fromCheckpoint(filter: Filter, checkpoint: Long): F[Source[T, NotUsed]] = ???

        private def fromChangeStreamSource[Mat](source: Source[ChangeStreamDocument[Document], Mat]): Source[T, Mat] =
          source
            .flatMapConcat(change =>
              DocumentDecoder[T]
                .fromDocument(change.getFullDocument)
                .fold(_ => Source.empty, t => Source.single(t))
            )
      }
  }

  object Mock {

    def make[F[_]: Applicative, T, Filter]: MongoSubscriptionAlg[F, T, Filter] =
      new MongoSubscriptionAlg[F, T, Filter] {
        override def fromStart(filter: Filter): F[Source[T, NotUsed]] = ???

        override def fromCheckpoint(filter: Filter, checkpoint: Long): F[Source[T, NotUsed]] = ???
      }
  }
}
