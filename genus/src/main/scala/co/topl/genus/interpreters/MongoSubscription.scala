package co.topl.genus.interpreters

import akka.NotUsed
import akka.stream.alpakka.mongodb.scaladsl.MongoSource
import akka.stream.scaladsl.Source
import cats.Applicative
import cats.implicits._
import co.topl.genus.algebras.SubscriptionAlg
import co.topl.utils.mongodb.DocumentDecoder
import com.mongodb.client.model.changestream.ChangeStreamDocument
import org.mongodb.scala.bson.conversions.Bson
import org.mongodb.scala.model.Aggregates
import org.mongodb.scala.{Document, MongoCollection}

object MongoSubscription {

  object Eval {

    def make[F[_]: Applicative, T: DocumentDecoder](
      collection: MongoCollection[Document]
    ): SubscriptionAlg[F, Source[*, NotUsed], Bson, ChangeStreamDocument[T]] =
      (filter: Bson) =>
        MongoSource(collection.watch(Seq(Aggregates.filter(filter))))
          .flatMapConcat(change =>
            DocumentDecoder[T]
              .fromDocument(change.getFullDocument)
              .map(value =>
                new ChangeStreamDocument[T](
                  change.getOperationType,
                  change.getResumeToken,
                  change.getNamespaceDocument,
                  change.getDestinationNamespaceDocument,
                  value,
                  change.getDocumentKey,
                  change.getClusterTime,
                  change.getUpdateDescription,
                  change.getTxnNumber,
                  change.getLsid
                )
              )
              .map(Source.single)
              .getOrElse(Source.empty)
          )
          .pure[F]
  }
}
