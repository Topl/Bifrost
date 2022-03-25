package co.topl.genus.interpreters

import cats.implicits._
import akka.NotUsed
import akka.stream.scaladsl.Source
import cats.Applicative
import cats.data.Chain
import co.topl.genus.algebras.MongoQuery
import co.topl.genus.services.services_types.Paging
import co.topl.genus.typeclasses.{MongoFilter, MongoSort}

object MockMongoQuery {

  def make[F[_]: Applicative, T](values: Chain[T]): MongoQuery[F, T] =
    new MongoQuery[F, T] {

      override def query[Filter: MongoFilter, Sort: MongoSort](
        filter: Filter,
        sort:   Sort,
        paging: Option[Paging]
      ): F[Source[T, NotUsed]] = Source(values.toList).pure[F]
    }
}
