package co.topl.genus.interpreters

import akka.NotUsed
import akka.stream.alpakka.mongodb.scaladsl.MongoSource
import akka.stream.scaladsl.Source
import cats.implicits._
import cats.{Applicative, MonadError, MonadThrow}
import co.topl.genus.algebras.DataStoreQueryAlg
import co.topl.genus.services.services_types.Paging
import co.topl.genus.typeclasses.MongoFilter
import co.topl.utils.mongodb.DocumentDecoder
import org.mongodb.scala.bson.conversions.Bson
import org.mongodb.scala.{Document, MongoCollection}
import co.topl.genus.typeclasses.implicits._

object MongoQueryInterp {

  type MongoQueryAlg[F[_], T, Filter] = DataStoreQueryAlg[F, Source[*, NotUsed], Bson, Filter, T]

  object Eval {

    /**
     * Makes an instance of a MongoDB-specific query algebra.
     *
     * @param collection the mongo collection to connect to and query documents from
     * @tparam F a functor with a `MonadThrow` instance for catching thrown errors
     * @tparam T the type of document to query for with an instnace of `DocumentDecoder`
     * @return a new instance of the query algebra which can query a mongo collection
     */
    def make[F[_]: MonadThrow, T: DocumentDecoder, Filter: MongoFilter](
      collection: MongoCollection[Document]
    ): MongoQueryAlg[F, T, Filter] =
      (filter: Filter, sort: Bson, paging: Option[Paging]) =>
        for {
          // catch error with finding and sorting on a mongo collection
          queryRequest    <- MonadThrow[F].catchNonFatal(collection.find(filter.toBsonFilter).sort(sort))
          queryWithPaging <-
            // catch error with setting a limit and skip value
            MonadThrow[F].catchNonFatal(
              paging
                .map(opts => queryRequest.limit(opts.pageSize).skip(opts.pageSize * opts.pageNumber))
                .getOrElse(queryRequest)
            )
          // catch error with creating a Mongo Source
          documentsSource <- MonadThrow[F].catchNonFatal(MongoSource(queryWithPaging))
          // ignore any documents that fail to decode by using a flatmap from document to Source[T] where
          // the source is empty for failed documents
          querySource = documentsSource.flatMapConcat(document =>
            DocumentDecoder[T]
              .fromDocument(document)
              .map(Source.single)
              .getOrElse(Source.empty)
          )
        } yield querySource
  }

  object Mock {

    def make[F[_]: Applicative, T, Filter]: MongoQueryAlg[F, T, Filter] =
      (_: Filter, _: Bson, _: Option[Paging]) => Source.empty[T].pure[F]
  }
}
