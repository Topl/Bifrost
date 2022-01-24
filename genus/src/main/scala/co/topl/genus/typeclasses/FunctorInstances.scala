package co.topl.genus.typeclasses

import akka.NotUsed
import akka.stream.scaladsl.Source
import cats.implicits._
import cats.Functor
import co.topl.genus.algebras.DataStoreQueryAlg
import co.topl.genus.services.services_types.Paging

trait FunctorInstances {

  implicit val sourceFunctor: Functor[Source[*, NotUsed]] = new Functor[Source[*, NotUsed]] {
    override def map[A, B](fa: Source[A, NotUsed])(f: A => B): Source[B, NotUsed] = fa.map(f)
  }

  implicit def dataStoreQueryAlgFunctor[F[_]: Functor, G[_]: Functor, Sort, Filter, *]
    : Functor[DataStoreQueryAlg[F, G, Sort, Filter, *]] =
    new Functor[DataStoreQueryAlg[F, G, Sort, Filter, *]] {

      override def map[A, B](fa: DataStoreQueryAlg[F, G, Sort, Filter, A])(
        f:                       A => B
      ): DataStoreQueryAlg[F, G, Sort, Filter, B] =
        (filter: Filter, sort: Sort, paging: Option[Paging]) =>
          fa
            .query(filter, sort, paging)
            .map(_.map(f))
    }
}
