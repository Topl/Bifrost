package co.topl.genus.typeclasses

import akka.stream.scaladsl.Source
import cats.Functor

trait FunctorInstances {

  implicit def sourceFunctor[Mat]: Functor[Source[*, Mat]] = new Functor[Source[*, Mat]] {

    override def map[A, B](fa: Source[A, Mat])(f: A => B): Source[B, Mat] =
      fa.map(f)
  }
}
