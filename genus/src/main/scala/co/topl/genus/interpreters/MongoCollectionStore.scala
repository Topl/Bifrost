package co.topl.genus.interpreters

import akka.stream.scaladsl.Source
import cats.Applicative
import cats.data.OptionT
import cats.implicits._
import co.topl.genus.algebras.MongoStore
import org.mongodb.scala.bson.conversions.Bson
import org.mongodb.scala.{Document, FindObservable, MongoCollection}

object MongoCollectionStore {

  def make[F[_]: Applicative](collection: MongoCollection[Document]): MongoStore[F] =
    (filtering: Option[Bson], sorting: Option[Bson], limit: Option[Int], skip: Option[Int]) =>
      Source
        .fromPublisher(
          OptionT(
            List(
              filtering.map(f => (x: FindObservable[Document]) => x.filter(f)),
              sorting.map(s => (x: FindObservable[Document]) => x.sort(s)),
              limit.map(l => (x: FindObservable[Document]) => x.limit(l)),
              skip.map(s => (x: FindObservable[Document]) => x.skip(s))
            )
          ).foldLeft(collection.find())((state, applyOption) => applyOption(state))
        )
        .pure[F]
}
