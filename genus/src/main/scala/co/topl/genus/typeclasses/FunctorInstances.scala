package co.topl.genus.typeclasses

import akka.stream.scaladsl.Source
import cats.Functor
import cats.implicits._
import co.topl.genus.interpreters.MongoQueryInterp.MongoQueryAlg
import co.topl.genus.services.services_types.Paging
import org.mongodb.scala.bson.conversions.Bson

trait FunctorInstances {

  implicit def sourceFunctor[Mat]: Functor[Source[*, Mat]] = new Functor[Source[*, Mat]] {

    override def map[A, B](fa: Source[A, Mat])(f: A => B): Source[B, Mat] =
      fa.map(f)
  }

  implicit def mongoQueryAlgFunctor[F[_]: Functor, Filter]: Functor[MongoQueryAlg[F, *, Filter]] =
    new Functor[MongoQueryAlg[F, *, Filter]] {

      override def map[A, B](fa: MongoQueryAlg[F, A, Filter])(f: A => B): MongoQueryAlg[F, B, Filter] =
        (filter: Filter, sort: Bson, paging: Option[Paging]) => fa.query(filter, sort, paging).map(_.map(f))
    }
}
