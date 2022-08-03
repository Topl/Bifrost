package co.topl.genus.interpreters

import akka.stream.scaladsl.Source
import cats.Applicative
import cats.data.OptionT
import cats.implicits._
import co.topl.genus.algebras.MongoStore
import org.mongodb.scala.bson.conversions.Bson
import org.mongodb.scala.{Document, FindObservable, MongoCollection}

object MongoCollectionStore {

  /**
   * Makes a new Mongo Store using the provided Mongo DB collection for making queries.
   * @param collection the collection to send queries to
   * @tparam F the effect-ful type to perform queries in
   * @return a new instance of a [[MongoStore]]
   */
  def make[F[_]: Applicative](collection: MongoCollection[Document]): MongoStore[F] =
    (filtering: Option[Bson], sorting: Option[Bson], limit: Option[Int], skip: Option[Int]) =>
      Source
        .fromPublisher(
          OptionT
            .apply(
              // create a list of functions which will apply options to a `find` query
              List(
                filtering.map(f => (x: FindObservable[Document]) => x.filter(f)),
                sorting.map(s => (x: FindObservable[Document]) => x.sort(s)),
                limit.map(l => (x: FindObservable[Document]) => x.limit(l)),
                skip.map(s => (x: FindObservable[Document]) => x.skip(s))
              )
            )
            // iterate over every non-empty function and apply the option to the query
            .foldLeft(collection.find())((query, applyOption) => applyOption(query))
        )
        .pure[F]
}
