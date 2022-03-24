package co.topl.genus.typeclasses

import akka.NotUsed
import akka.stream.scaladsl.Source
import cats.Functor
import cats.implicits._
import co.topl.genus.interpreters.MongoQueryInterp.MongoQueryAlg
import co.topl.genus.interpreters.MongoSubscriptionInterp.MongoSubscriptionAlg
import co.topl.genus.services.services_types.Paging
import co.topl.genus.types.BlockHeight
import org.mongodb.scala.bson.conversions.Bson

trait FunctorInstances {

  implicit def sourceFunctor[Mat]: Functor[Source[*, Mat]] = new Functor[Source[*, Mat]] {

    override def map[A, B](fa: Source[A, Mat])(f: A => B): Source[B, Mat] =
      fa.map(f)
  }
}
