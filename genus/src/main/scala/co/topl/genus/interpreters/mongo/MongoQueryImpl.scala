package co.topl.genus.interpreters.mongo

import akka.NotUsed
import akka.stream.alpakka.mongodb.scaladsl.MongoSource
import akka.stream.scaladsl.Source
import cats.MonadThrow
import cats.implicits._
import co.topl.genus.algebras.MongoQuery
import co.topl.genus.services.services_types.Paging
import co.topl.genus.typeclasses.implicits._
import co.topl.genus.typeclasses.{MongoFilter, MongoSort}
import org.mongodb.scala.{Document, MongoCollection}

object MongoQueryImpl {

  /**
   * Makes an instance of a MongoDB-specific query algebra.
   *
   * @param collection the mongo collection to connect to and query documents from
   * @tparam F a functor with a `MonadThrow` instance for catching thrown errors
   * @tparam T the type of document to query for with an instnace of `DocumentDecoder`
   * @return a new instance of the query algebra which can query a mongo collection
   */
  def make[F[_]: MonadThrow](
    collection: MongoCollection[Document]
  ): MongoQuery[F] =
    new MongoQuery[F] {

      override def query[Filter: MongoFilter, Sort: MongoSort](
        filter: Filter,
        sort:   Sort,
        paging: Option[Paging]
      ): F[Source[Document, NotUsed]] =
        for {
          // catch error with finding and sorting on a mongo collection
          queryRequest    <- MonadThrow[F].catchNonFatal(collection.find(filter.toBsonFilter).sort(sort.toBsonSorting))
          queryWithPaging <-
            // catch error with setting a limit and skip value
            MonadThrow[F].catchNonFatal(
              paging
                .map(opts => queryRequest.limit(opts.pageSize).skip(opts.pageSize * opts.pageNumber))
                .getOrElse(queryRequest)
            )
          // catch error with creating a Mongo Source
          documentsSource <- MonadThrow[F].catchNonFatal(MongoSource(queryWithPaging))
        } yield documentsSource
    }
}
